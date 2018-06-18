

#'
#'
#'


### 0. Dependencies ----
source("0__common.R")
job <- job.startup("preprocessing_data")
suppressPackageStartupMessages({
  library(psych)
  library(ggplot2)
  library(corrplot)
  
  library(parallel)
  library(foreach)
  
  library(lightgbm)
})
source("common_modeling.R")
source("credit_modeling.R")



### 1. Load datasets ----
datasets <- list(
    Train = fread.csv.zipped("application_train.csv", job$Config$DataDir),
    Test = fread.csv.zipped("application_test.csv", job$Config$DataDir) %>% mutate(Target = NA_integer_)
  ) %>% 
  map(
    ~ .x %>% rename(Label = Target)
  )
  
stopifnot(
  length(datasets) == 2,
  nrow(datasets$Test) > 0,
  nrow(datasets$Train) > nrow(datasets$Test),
  is_empty(setdiff(names(datasets$Train), names(datasets$Test))),
  !anyNA(datasets$Train$Label)
)


data.columns.desc <- fread(sprintf("%s/HomeCredit_columns_description.csv", job$Config$DataDir))



### 2. Preprocessing data ----
metadata <- model.getMetadata(datasets$Train)


## 2.1. Format features
wdays <- c(5, 2, 6, 1, 4, 3, 7)
names(wdays) <- c("friday", "tuesday", "saturday", "monday", "thursday", "wednesday", "sunday")
missingValueLabel <- -1L


datasets <- datasets %>% 
  map(
    ~ .x %>% 
      ## logic features processing
      mutate_at(
        setdiff(metadata$Features$Logic, names(.x %>% select_if(is.integer))),
        funs(
          if_else(!is.na(.),
                  if_else(. %in% c("M", "Y", "Yes"), 1L, 0L),
                  missingValueLabel)
        )
      ) %>% 
      ## categorical features processing
      # OHE features
      mutate_at(
        metadata$Features$FactorOHE,
        funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), as.character(missingValueLabel)))
      ) %>% 
      mutate(
        WeekdayApprProcessStart = wdays[WeekdayApprProcessStart],
        HousetypeMode = if_else(HousetypeMode != as.character(missingValueLabel), HousetypeMode, "block_of_flats"),
        FondkapremontMode = if_else(FondkapremontMode != as.character(missingValueLabel), FondkapremontMode, "reg_oper_account")
      ) %>% 
      ## numeric features processing
      mutate_at(
        metadata$Features$Numeric,
        funs(ifelse(!is.na(.), ., 0))
      )
)


## 2.2. Split data
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
encoders.OH <- metadata$Features$FactorOHE %>% 
  map(~ common.modeling.getOneHotEncoder(.x, datasets$Train))

encoders.SLE <- metadata$Features$FactorSLE %>% 
  map(~ common.modeling.smoothedLikelihoodEncoding(.x, datasets$Train))


# apply encoders
datasets <- datasets %>%
  map(~ common.modeling.applyEncoders(.x, encoders.OH, metadata$Features$FactorOHE)) %>%
  map(~ common.modeling.applyEncoders(.x, encoders.SLE, metadata$Features$FactorSLE))

stopifnot(
  length(datasets) == 3,
  nrow(datasets$Train) > nrow(datasets$Valid),
  nrow(datasets$Valid) > nrow(datasets$Test),
  nrow(datasets$Test) > 0
)


## replace missing values

# for SLE-encoded features
factorSLE.pattern <- sprintf("^(%s)+_SL", paste(metadata$Features$FactorSLE, collapse = "|"))

datasets <- datasets %>% 
  map(
    ~ common.modeling.replaceNA(.x, datasets$Train %>% select(matches(factorSLE.pattern)), median)
  )

# for OH-encoded features # warn: transformation modify not only OHE features
factorOHE.pattern <- sprintf("^(%s)+_", paste(metadata$Features$FactorOHE, collapse = "|"))
datasets <- datasets %>% 
  map(
    ~ .x %>% mutate_at(vars(matches(factorOHE.pattern)), funs(ifelse(!is.na(.), ., 0)))
  )

stopifnot(
  all(
    list(
        metadata$Features$FactorSLE,
        metadata$Features$FactorOHE
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



### 3. EDA ----
cor(x[, 1:20]) > abs(.85)
corrplot(cor(x[, 1:20]), method = "color", type="upper", order="hclust")

ggplot(datasets$Train, aes(x = AmtReqCreditBureauQrt , fill = Target)) +
  geom_histogram(bins = 10)





### 4. Train model ----
## convert to lgb datasets
categoricalFeatures <- NULL # one-hot encoding

mTrain <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Train, metadata),
                      label = datasets$Train$Label)

mTest <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Valid, metadata),
                     label = datasets$Valid$Label)

## compute hyperparams
gridSearch <- common.modeling.getHyperparams(.size = 10L) %>% 
  common.modeling.selectHyperparams(mTrain, mTest, ., .nrounds = 6L)


## train and predict model
modelParams <- gridSearch[1, ] %>% select(-starts_with("AUC")) %>% as.list
model <- common.modeling.train(mTrain, NULL, modelParams, 20L)

predictions <- datasets %>%
  map(
    ~ .x %>% 
      select(
        SkIdCurr, Label
      ) %>% 
      cbind(
        ., Score = predict(model, common.modeling.convertToMatrix(.x, metadata))
      )
  )



### 5. Eval model ----

metrics <- list(
  Perfomance = map(list(predictions$Train, predictions$Valid),
                   ~ common.modeling.evaluateModel(.x %>% select(Label, Score))),
  Hyperparams = modelParams,
  FeatureImportance = lgb.importance(model, percentage = T) %>% head(50),
  Metadata = metadata
)

common.modeling.saveArtifacts(
  "homecreditdefault", 
  model, 
  predictions$Test  %>% transmute(SK_ID_CURR = SkIdCurr, TARGET = Score),
  metrics, 
  gridSearch, 
  job$Config$OutputDir)





### 6. Reflection ----
ggplot(predictions$Valid, aes(Score)) +
  geom_density(alpha = .4) +
  geom_vline(xintercept = .5, linetype = "dashed", color = "darkred") +
  labs(title = "Плотность распределения риска", subtitle = "Вероятность получить оценку риска для отсечек 50%, 75%, 85% от общего количества транзакций",
       x = "Риск", y = "Вероятность получения", caption = "(с) 2018, OpenWay") +
  theme_minimal()

# see https://github.com/Microsoft/LightGBM/blob/master/R-package/R/lgb.plot.interpretation.R
tree_interpretation <- lgb.interprete(model, common.modeling.convertToMatrix(datasets$Test, metadata), 1:100)
lgb.plot.interpretation(tree_interpretation[[1]], top_n = 30)

View(
  predictions$Valid %>% 
    mutate(Diff = abs(Score - Label)) %>% 
    arrange(-Diff)
)






