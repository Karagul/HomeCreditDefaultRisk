

#'
#'
#'


### 0. Dependencies ----
source("0__common.R")
job <- job.startup("preprocessing_data")
suppressPackageStartupMessages({
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

wdays <- c(5, 2, 6, 1, 4, 3, 7); names(wdays) <- c("friday", "tuesday", "saturday", "monday", "thursday", "wednesday", "sunday")

datasets <- datasets %>% 
  map(~ .x %>% 
        ## clean logic errors
        mutate(
          CodeGender = if_else(CodeGender != "XNA", CodeGender, NA_character_), # XNA is invalid gender
          OrganizationType = if_else(OrganizationType != "XNA", OrganizationType, NA_character_),
          NameIncomeType = if_else(NameIncomeType != "Maternity leave", NameIncomeType, NA_character_), # there is no 'Maternity leave' in test dataset
          NameFamilyStatus = if_else(NameFamilyStatus != "Unknown", NameFamilyStatus, NA_character_),
          OwnCarAge = if_else(OwnCarAge %in% c(64, 65), NA_real_, OwnCarAge),
          DaysEmployed = if_else(DaysEmployed == 365243, NA_integer_, DaysEmployed)
        ) %>% 
        ## logic features processing
        mutate_at(
          setdiff(metadata$Features$Logic, names(.x %>% select_if(is.integer))),
          funs(if_else(!is.na(.),
                       if_else(. %in% c("Y", "Yes"), 1L, 0L),
                       NA_integer_)
          )
        ) %>%  
        ## OHE features
        mutate_at(
          metadata$Features$FactorOHE,
          funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
        ) %>% 
        mutate(
          WeekdayApprProcessStart = wdays[tolower(WeekdayApprProcessStart)],
          HousetypeMode = if_else(!is.na(HousetypeMode), HousetypeMode, "block_of_flats"), # replace to mode value
          FondkapremontMode = if_else(!is.na(FondkapremontMode), FondkapremontMode, "reg_oper_account") # replace to mode value
        ) %>% 
        ## processing missing data
        # numeric
        mutate_at(
          metadata$Features$Numeric,
          funs("missing" = if_else(is.na(.), 1L, 0L))
        ) %>% 
        mutate_at(
          metadata$Features$Numeric,
          funs(ifelse(!is.na(.), ., 0))
        ) %>% 
        # logic
        mutate_at(
          metadata$Features$Logic,
          funs("missing" = if_else(is.na(.), 1L, 0L))
        ) %>%
        mutate_at(
          metadata$Features$Logic, # all must be integer
          funs(if_else(!is.na(.), ., -1L))
        ) %>%
        # factor
        mutate_at(
          metadata$Features$FactorOHE,
          funs("missing" = if_else(is.na(.), 1L, 0L))
        ) %>%
        mutate_at(
          metadata$Features$FactorOHE, # all must be character
          funs(if_else(!is.na(.), ., "none"))
        ) %>%
        ## remove redundant
        select(
          -one_of(metadata$Features$Redundant)
        )
      )



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



### 4. Train model ----
## convert to lgb datasets
categoricalFeatures <- NULL # one-hot encoding

mTrain <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Train, metadata),
                      label = datasets$Train$Label)

mTest <- lgb.Dataset(data = common.modeling.convertToMatrix(datasets$Valid, metadata),
                     label = datasets$Valid$Label)

## compute hyperparams
gridSearch <- common.modeling.getHyperparams(.size = 200L,
                                             .learning_rate = c(.06, .08),
                                             .max_depth = -1L,
                                             .max_bin = 255L,
                                             .num_leaves = c(47, 55),
                                             .min_data_in_leaf = c(4, 6), 
                                             .min_sum_hessian_in_leaf = c(.001),
                                             .feature_fraction = c(.5, .55, .6),
                                             .bagging_fraction = c(.9, .95, 1),
                                             .bagging_freq = 6,
                                             .lambda_l1 = c(0.001, 0.01),
                                             .lambda_l2 = c(0.001, 0.01),
                                             .min_split_gain = 0,
                                             .scale_pos_weight = c(8, 10, 12)
                                             ) %>% 
  common.modeling.selectHyperparams(mTrain, mTest, ., .nrounds = 32L)


## train model
gridSearchX <- gridSearch %>% filter(AUC_diff < .02)
modelParams <- gridSearchX[1, ] %>% select(-starts_with("AUC")) %>% as.list
modelParams$learning_rate <- .01
model <- common.modeling.train(mTrain, NULL, modelParams, 2e3)


## predict
predictions <- datasets %>%
  map(~ .x %>% 
        select(SkIdCurr, Label) %>% 
        cbind(., Score = predict(model, common.modeling.convertToMatrix(.x, metadata)))
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
  Metadata = metadata
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
ggplot(predictions$Valid) +
  geom_density(aes(x = Score, color = factor(Label)), alpha = .4) +
  geom_vline(xintercept = .5, linetype = "dashed", color = "darkred") +
  labs(title = "", subtitle = "", x = "", y = "", caption = "") +
  theme_bw()

# see https://github.com/Microsoft/LightGBM/blob/master/R-package/R/lgb.plot.interpretation.R
tree_interpretation <- lgb.interprete(model, common.modeling.convertToMatrix(datasets$Test, metadata), 1:100)
lgb.plot.interpretation(tree_interpretation[[1]], top_n = 30)

View(
  predictions$Valid %>% 
    mutate(Diff = abs(Score - Label)) %>% 
    arrange(-Diff)
)


