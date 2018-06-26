

#'
#'
#'


### 0. Dependencies ----
source("0__common.R")
job <- job.startup("preprocessing_data")

suppressPackageStartupMessages({
  library(data.table)
  
  library(parallel)
  library(foreach)
  
  library(lightgbm)
})

source("loan.R")
source("previous_loan.R")
source("bureau.R")

source("common_modeling.R")



### 1. Load datasets ----
## Colums description
data.columns.desc <- fread(sprintf("%s/HomeCredit_columns_description.csv", job$Config$DataDir))


## Bureau dataset
bureau.history <- bureau.load(job$Config) %>% bureau.getHistory # readRDS("cache/bureau.history.rds")
saveRDS(bureau.history, "cache/bureau.history.rds")

bureau.history.stats <- bureau.getHistoryStats(bureau.history, .replaceNA = 0)
stopifnot(
  !anyNA(bureau.history.stats)
)



## Previous loan applictions
prevLoans <- prevLoan.load(job$Config) %>% 
  prevLoan.filter %>% 
  prevLoan.preprocessing

prevLoans.history <- prevLoan.getHistory(prevLoans)
saveRDS(prevLoans.history, "cache/prevLoans.history.rds")

prevLoans.history.stats <- prevLoan.getHistoryStats(prevLoans.history, .replaceNA = 0)
stopifnot(
  !anyNA(prevLoans.history.stats)
)



## Loans dataset
loans <- loan.load(job$Config)



### 2. Preprocessing data ----
loan.metadata <- loan.getMetadata(loans$Train)

loans <- loans %>% 
  map(~ .x %>% 
        loan.clear(., loan.metadata) %>% 
        loan.format(., loan.metadata) %>% 
        loan.calcRequestsNumber %>% 
        loan.calcDocumentNumber %>% 
        loan.calcDefaultSocialCircleNumber %>% 
        loan.processingOwnership %>% 
        loan.processingIncome %>% 
        loan.processingExternalSourceScore %>% 
        loan.processingDays %>% 
        loan.missingValuesProcessing(., loan.metadata)
      )



# join w/ bureau
datasets <- loans %>% 
  map(~ .x %>% left_join(prevLoans.history.stats, by = "SkIdCurr"))
  # map(~ .x %>% left_join(bureau.history.stats, by = "SkIdCurr"))

stopifnot(
  length(datasets) == length(loans),
  nrow(datasets$Train) == nrow(loans$Train),
  nrow(datasets$Test) == nrow(loans$Test)
)

redundantFieldsIndx <- common.modeling.getRedundantFields(datasets$Test)
redundantFieldsNames <- setdiff(names(datasets$Test)[redundantFieldsIndx], names(loans$Test))

stopifnot(
  length(redundantFieldsNames) > 0,
  !any(setdiff(redundantFieldsNames, names(datasets$Test)))
)

datasets <- datasets %>% 
  map(~ .x %>% select(-one_of(redundantFieldsNames)))



## 2.2. Split data
#! todo: create folds
sDatasets <- common.modeling.splitDataset(datasets$Train, "SkIdCurr")
datasets$Train <- sDatasets$Train
datasets$Valid <- sDatasets$Valid

stopifnot(
  length(datasets) == 3,
  nrow(datasets$Train) > nrow(datasets$Valid),
  nrow(datasets$Valid) > nrow(datasets$Test),
  nrow(datasets$Test) > 0
)
rm(sDatasets)


## 2.3. Features encoding
# calc encoders
encoders.OH <- loan.metadata$Features$FactorOHE %>% 
  map(~ common.modeling.getOneHotEncoder(.x, datasets$Train))

encoders.SLE <- loan.metadata$Features$FactorSLE %>% 
  map(~ common.modeling.smoothedLikelihoodEncoding(.x, datasets$Train))


# apply encoders
datasets <- datasets %>%
  map(~ common.modeling.applyEncoders(.x, encoders.OH, loan.metadata$Features$FactorOHE)) %>%
  map(~ common.modeling.applyEncoders(.x, encoders.SLE, loan.metadata$Features$FactorSLE))

stopifnot(
  length(datasets) == 3,
  nrow(datasets$Train) > nrow(datasets$Valid),
  nrow(datasets$Valid) > nrow(datasets$Test),
  nrow(datasets$Test) > 0
)


## replace missing values

# for SLE-encoded features
factorSLE.pattern <- sprintf("^(%s)+_SL", paste(loan.metadata$Features$FactorSLE, collapse = "|"))

datasets <- datasets %>% 
  map(
    ~ common.modeling.replaceNA(.x, datasets$Train %>% select(matches(factorSLE.pattern)), median)
  )

# for OH-encoded features # warn: transformation modify not only OHE features
factorOHE.pattern <- sprintf("^(%s)+_", paste(loan.metadata$Features$FactorOHE, collapse = "|"))
datasets <- datasets %>% 
  map(
    ~ .x %>% mutate_at(vars(matches(factorOHE.pattern)), funs(ifelse(!is.na(.), ., 0)))
  )

stopifnot(
  all(
    list(
        loan.metadata$Features$FactorSLE,
        loan.metadata$Features$FactorOHE
      ) %>% 
      map2(
        list(factorSLE.pattern, factorOHE.pattern),
        ~ if (is_empty(.x)) {
          datasets %>% map_lgl(~ T)
        } else {
          datasets %>% 
            map_lgl(
              function(..x) nrow(..x %>% select(matches(.y)) %>% filter_all(any_vars(is.na(.)))) == 0
            )
        }
      ) %>% 
      as_vector
  )
)

rm(factorSLE.pattern)
rm(factorOHE.pattern)



### 4. Train model ----
## convert to lgb datasets
categoricalFeatures <- NULL # one-hot encoding

datasets <- datasets.origin %>% 
  map(~ .x %>% 
        select(
          -ends_with("Avg"), -ends_with("Mode"), -ends_with("Medi")
          -ExtSource1, -ExtSource2, -AmtGoodsPrice
        ))

mTrain <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Train, loan.metadata),
                      label = datasets$Train$Label, 
                      categorical_feature = categoricalFeatures)

mTest <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Valid, loan.metadata),
                     label = datasets$Valid$Label, 
                     categorical_feature = categoricalFeatures)

## compute hyperparams
gridSearch <- common.modeling.getHyperparams(.size = 20L,
                                             .learning_rate = c(.06, .08),
                                             .max_depth = 8L,
                                             .max_bin = 255L,
                                             .num_leaves = c(47, 55),
                                             .min_data_in_leaf = c(4, 6), 
                                             .min_sum_hessian_in_leaf = c(.001),
                                             .feature_fraction = c(.55, .65, .75),
                                             .bagging_fraction = c(.9, .95, 1),
                                             .bagging_freq = 6,
                                             .lambda_l1 = c(0.001, 0.01),
                                             .lambda_l2 = c(0.001, 0.01),
                                             .min_split_gain = 0,
                                             .scale_pos_weight = c(8, 10, 12)
                                             ) %>% 
  common.modeling.selectHyperparams(mTrain, mTest, ., .nrounds = 32L)

View(gridSearch)

## train model
modelParams <- gridSearch[1, ] %>% select(-starts_with("AUC")) %>% as.list
modelParams$learning_rate <- .01


# TODO: CV
model <- common.modeling.train(mTrain, NULL, modelParams, 1e4)


## predict
predictions <- datasets %>%
  map(~ .x %>% 
        select(SkIdCurr, Label) %>% 
        cbind(., Score = predict(model, common.modeling.convertToMatrix(.x, loan.metadata)))
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
                   ~ common.modeling.evaluateModel(.x %>% select(Label, Score))),
  Hyperparams = modelParams,
  FeatureImportance = lgb.importance(model, percentage = T) %>% head(50),
  Metadata = loan.metadata
)

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
tree_interpretation <- lgb.interprete(model, common.modeling.convertToMatrix(datasets$Test, loan.metadata), 1:100)
lgb.plot.interpretation(tree_interpretation[[1]], top_n = 30)

View(
  predictions$Valid %>% 
    mutate(Diff = abs(Score - Label)) %>% 
    arrange(-Diff)
)


