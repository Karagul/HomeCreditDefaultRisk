

#'
#' Default Model Kernel
#'


### 0. Dependencies ----
source("0__common.R")
job <- job.startup("default_model_kernal")

suppressPackageStartupMessages({
  library(lightgbm)
})



### 1. Load/preprocessing datasets ----
source("datasets_loader.R")

data.columns.desc <- fread(sprintf("%s/HomeCredit_columns_description.csv", job$Config$DataDir))

loans <- loader.loans()
bureau <- loader.bureau()
prevLoans <- loader.prevLoans()


### 2. Feature engineering ----

## 2.1. Join datasets
loans.metadata <- loan.getMetadata(loans$Train)

datasets <- loans %>%
  map(
    ~ .x %>% 
        left_join(bureau, by = loans.metadata$Key) %>% 
        left_join(prevLoans, by = loans.metadata$Key)
    )

stopifnot(
  length(datasets) == length(loans),
  nrow(datasets$Train) == nrow(loans$Train),
  nrow(datasets$Test) == nrow(loans$Test),
  !anyDuplicated(datasets$Test$SkIdCurr),
  !anyDuplicated(datasets$Train$SkIdCurr)
)



### 3. Train model ----

## prepare datasets
source("default_modeling.R")

grid <- common.modeling.getHyperparams(.size = 100L,
                                       .learning_rate = c(.08),
                                       .max_depth = c(7:8),
                                       .max_bin = c(53, 127, 255),
                                       .num_leaves = c(47),
                                       .min_data_in_leaf = c(2, 4),
                                       .min_sum_hessian_in_leaf = c(.001, .01),
                                       .feature_fraction = c(.85, .90, .95),
                                       .bagging_fraction = c(.9, .95, 1),
                                       .bagging_freq = 6,
                                       .lambda_l1 = c(.064, .128, .256),
                                       .lambda_l2 = c(.064, .128, .256),
                                       .min_split_gain = 0,
                                       .scale_pos_weight = c(4:6)
                                       )

grid <- gridSearch %>% top_n(10, AUC_test) %>% select(-Id, -starts_with("AUC"))

datasets.orig <- datasets

interations_num <- 1L
cv_results <- list(interations_num)

for (i in 1:interations_num) {
  write(sprintf("CV iteration #%s is starting...", i), stdout())
  
  ## prepare dataset
  datasets <- datasets.orig %>% 
    splitDataset(., loans.metadata) %>%  
    encodingFeatures(., loans.metadata) %>% 
    replaceMissingValues(., loans.metadata, "(Avg|Mode|Medi)$", median) %>% 
    replaceMissingValues(., loans.metadata, "(ExtSource\\d)$", mean) 
  
  
  ## convert to lgb datasets
  mTrain <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Train, loans.metadata),
                        label = datasets$Train$Label)
  
  mTest <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Valid, loans.metadata),
                       label = datasets$Valid$Label)
  
  
  ## compute hyperparams
  gridSearch <- common.modeling.selectHyperparams(mTrain, mTest, grid, .nrounds = 32L)
  View(gridSearch)
  cv_results[[i]] <- gridSearch %>% select(Id, starts_with("AUC"))
  
  
  ## GC
  rm(datasets)
  rm(mTrain)
  rm(mTest)
  
  gc()
}


gridSearch <- cv_results %>%
    bind_cols %>% 
    select(Id, contains("AUC_test")) %>% 
    gather(key, measure, -Id) %>% 
    group_by(Id) %>% 
    summarise(
      AUC_min = min(measure),
      AUC_mean = mean(measure),
      AUC_max = max(measure),
      AUC_sd = sd(measure)
    ) %>% 
    bind_cols(grid) %>% 
    arrange(-AUC_mean)

View(gridSearch)


## train model
modelParams <- gridSearch %>% top_n(1, AUC_mean) %>% select(-Id, -starts_with("AUC")) %>% as.list
modelParams$learning_rate <- .01

model <- common.modeling.train(mTrain, NULL, modelParams, .nrounds = 2e3L)


## predict
predictions <- datasets %>%
  map(~ .x %>% 
        select(SkIdCurr, Label) %>% 
        cbind(., Score = predict(model, common.modeling.convertToMatrix(.x, loans.metadata)))
  )

stopifnot(
  nrow(predictions$Test) == 48744,
  !anyNA(predictions$Test %>% select(-Label)),
  nrow(predictions$Test %>% filter(Score < 1e-3)) == 0,
  nrow(predictions$Test %>% filter(Score > .999)) == 0
)


### 5. Eval model ----
metrics <- list(
  Perfomance = map(list(Train = predictions$Train, Valid = predictions$Valid),
                   ~ common.modeling.evaluateModel(.x %>% transmute(Label, Score))),
  Hyperparams = modelParams,
  FeatureImportance = lgb.importance(model, percentage = T) %>% head(50),
  Metadata = loans.metadata
)

metrics[["Perfomance"]][["Train"]][["ROC_AUC"]]
metrics[["Perfomance"]][["Valid"]][["ROC_AUC"]]


common.modeling.saveArtifacts(
  "homecreditdefault", 
  model, 
  predictions$Test %>% transmute(SK_ID_CURR = SkIdCurr, TARGET = Score),
  metrics, 
  gridSearch, 
  job$Config$OutputDir)



### 6. Reflection ----
library(ggplot2)
ggplot(predictions$Train) +
  geom_density(aes(x = Score, color = factor(Label)), alpha = .4) +
  geom_vline(xintercept = .5, linetype = "dashed", color = "grey") +
  labs(title = "", subtitle = "", x = "", y = "", caption = "") +
  theme_bw()

# see https://github.com/Microsoft/LightGBM/blob/master/R-package/R/lgb.plot.interpretation.R
tree_interpretation <- lgb.interprete(model, common.modeling.convertToMatrix(datasets$Test, loans.metadata), 1:100)
lgb.plot.interpretation(tree_interpretation[[1]], top_n = 30)

View(
  predictions$Valid %>% 
    mutate(Diff = abs(Score - Label)) %>% 
    arrange(-Diff)
)


