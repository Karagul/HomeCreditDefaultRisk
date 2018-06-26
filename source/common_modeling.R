

#' 
#' Common Modeling functions
#' 


# Preprocessig data ----

#' 
#'
#' @param dt 
#' @param .minSD 
#' @param .minNotNA 
#'
common.modeling.getRedundantFields <- function(dt, .filterSD = .01, .filterMissedRatio = .01) {
  require(dplyr)
  require(purrr)
  require(psych)
  
  stopifnot(
    is.data.frame(dt),
    is.numeric(.filterSD),
    is.numeric(.filterMissedRatio)
  )
  
  
  describe(dt) %>% 
    filter(sd < .filterSD | n/nrow(dt) < .filterMissedRatio) %>% 
    select(vars) %>% 
    as_vector
}




#' 
#'
#' @param dt 
#' @param .key 
#' @param .size 
#'
common.modeling.splitDataset <- function(dt, .key, .size = .8) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.character(.key),
    is.numeric(.size) && .size > 0 && .size < 1
  )
  
  result <- list(Train = dt %>% sample_frac(.size, replace = F))
  result$Valid <- dt %>% anti_join(result$Train, by = .key)
  
  stopifnot(
    length(result) == 2,
    all(map_lgl(result, ~ anyDuplicated(.x[.key]) == 0))
  )
  
  
  result
}



# Feature encodings ----

#' One-Hot encoding
#'
#' @param colName 
#' @param dt 
#' @param maxLevels 
#' 
common.modeling.getOneHotEncoder <- function(colName, dt, maxLevels = 100L) {
  require(stringr)
  require(dplyr)
  
  stopifnot(
    is.character(colName),
    is.data.frame(dt),
    is.numeric(maxLevels) && maxLevels > 0
  )
  
  
  factor.freq <- dt %>% 
    count_(colName) %>% 
    arrange(-n) %>% 
    dplyr::slice(1:maxLevels)
  
  f <- levels(factor(factor.freq[[colName]]))
  
  if (length(f) == 0) {
    stop("Column name not found")
  } else if (length(f) == 1) {
    stop("Column has invalid levels count")
  } else {
    stopifnot(length(f) <= maxLevels)
  }
  
  
  f <- addNA(f)
  convertFunc <- ifelse(is.integer(dt[[colName]]),
                        as.integer,
                        ifelse(is.numeric(dt[[colName]]), 
                               as.numeric,
                               as.character))
  
  result <- as.data.frame(
    t(model.matrix(~ f))
  ) %>%
    mutate(
      Key = c(convertFunc(as.character(f)), NA)
    )
  
  result[1, 3:ncol(result) - 1] <- 0
  
  colnames(result) <- c(paste(colName, result$Key, sep = "_"))
  colnames(result)[ncol(result)] <- colName
  
  stopifnot(
    nrow(result) == ncol(result),
    nrow(result) == length(f) + 1
  )
  
  result
}


local({
  library(dplyr)
  library(purrr)
  library(stringr)
  
  n_rows <- 1e3
  df <- data.frame(
    Id = sample(0:9, n_rows, replace = T),
    Name = sample(replicate(n_rows/10, paste(sample(LETTERS, 6, replace = T), collapse = "")), n_rows, replace = T),
    Label = sample(c(1L, 0L), n_rows, replace = T),
    stringsAsFactors = F
  )
  
  map(names(df)[[1]], ~ common.modeling.getOneHotEncoder(.x, df, 10))
  map(names(df), ~ common.modeling.getOneHotEncoder(.x, df, 25))
  map(names(df), ~ common.modeling.getOneHotEncoder(.x, df, 1000))
})



#' Smoothed Likelihood encoding
#'
#' @param colName 
#' @param dt 
#' @param alpha 
#' @param .minObservationN 
#'
#' @references https://tech.yandex.com/catboost/doc/dg/concepts/algorithm-main-stages_cat-to-numberic-docpage/
#' 
common.modeling.smoothedLikelihoodEncoding <- function(colName, dt, alpha = .05, .minObservationN = 30L) {
  require(dplyr)
  require(tidyr)
  stopifnot(
    is.character(colName),
    is.data.frame(dt) & !anyNA(dt$Label),
    is.numeric(alpha),
    is.numeric(.minObservationN) && .minObservationN > 0
  )
  
  
  feature.stats <- dt %>% 
    count_(c(colName, "Label")) %>% 
    spread(Label, n, fill = 0) %>% 
    inner_join(
      dt %>% count_(c(colName)), 
      by = colName
    )
  
  globalMean <- feature.stats %>% 
    filter(n >= .minObservationN) %>% 
    transmute(
      MeanTarget = (`1` + alpha)/(n + 1)
    )
  
  globalMean <- mean(globalMean$MeanTarget)
  stopifnot(
    !(is.na(globalMean) | is.nan(globalMean))
  )
  
  result <- feature.stats %>% 
    mutate(
      MeanTarget = ((`1` + alpha)/(n + 1) * n + globalMean * alpha)/(n + 1)
    ) %>% 
    select(
      one_of("MeanTarget", colName)
    )
  
  names(result) <- c(paste0(colName, "_SL"), colName)
  
  
  result
}


local({
  library(dplyr)
  library(purrr)
  library(stringr)
  
  n_rows <- 1e3
  df <- data.frame(
    Id = sample(0:9, n_rows, replace = T),
    Name = sample(replicate(n_rows/50, paste(sample(LETTERS, 6, replace = T), collapse = "")), n_rows, replace = T),
    Label = sample(c(1L, 0L), n_rows, replace = T),
    stringsAsFactors = F
  )
  
  df %>% count(Name, Label)
  
  x <- map(names(df %>% select(-Label)), ~ common.modeling.smoothedLikelihoodEncoding(.x, df))
  stopifnot(
    all(x[[1]][2]) %in% unique(df[[1]]),
    is.numeric((x[[1]][1]) %>% as_vector),
    !anyNA(x[[1]][1])
  )
  
  stopifnot(
    !any(setdiff(x[[2]][2] %>% as_vector, unique(df[[2]]))),
    is.numeric((x[[2]][1]) %>% as_vector),
    !anyNA(x[[2]][1])
  )
})



#' Apply encoders
#'
#' @param dt 
#' @param encodedFeatures 
#' @param encoders 
#' 
common.modeling.applyEncoders <- function(dt, encoders, encodedFeatures) {
  require(dplyr)
  require(purrr)
  
  stopifnot(
    is.data.frame(dt), 
    is_list(encoders) | is_empty(encoders),
    is_character(encodedFeatures) | is_empty(encodedFeatures)
  )
  
  
  if(!is_empty(encoders)) {
    map(encoders, function(.x) {
      dt %>% 
        left_join(
          .x, by = names(.x)[ncol(.x)]
        ) %>% 
        select(
          one_of(names(.x)[-ncol(.x)])
        )
    }) %>% 
      bind_cols %>% 
      cbind(dt, .) %>% 
      select(
        -one_of(encodedFeatures)
      )
  } else {
    dt
  }
}



#' Replace missing values to function calculation result
#'
#' @param .test 
#' @param .trainFeatures 
#' @param .f 
#'
common.modeling.replaceNA <- function(.test, .trainFeatures, .f) {
  stopifnot(
    is.data.frame(.test),
    is.data.frame(.trainFeatures),
    is.function(.f)
  )
  
  for(n in names(.trainFeatures)) {
    if (anyNA(.test[[n]])) {
      .test[is.na(.test[[n]]), ][[n]] <- .f(.trainFeatures[[n]], na.rm = T)
    }
  }
  
  .test
}



# Train and Score model ----

#' 
#'
#' @param dt 
#' @param .metadata 
#'
common.modeling.convertToMatrix <- function(dt, .metadata) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  
  as.matrix(
    dt %>% select(-Label, -one_of(.metadata$Features$ExtraVars))
  )
}



#'
#'
#' @param .size Grid size
#' @param .learning_rate 
#' @param .max_depth Maximum depth of tree (-1 means no limit)
#' @param .min_sum_hessian_in_leaf 
#' @param .feature_fraction Fraction of features
#' @param .bagging_fraction Fraction of data to train tree
#' @param .bagging_freq 
#' @param .lambda_l1 L1 regularization term on weights
#' @param .lambda_l2 L2 regularization term on weights
#' @param .min_split_gain The minimal gain to perform split 
#' @param .num_leaves 
#' @param .scale_pos_weight 
#' @param .max_bin Max number of bins that feature values will be bucketed in
#' @param .min_data_in_leaf 
#'
#' @details
#' - http://lightgbm.apachecn.org/en/latest/Parameters.html
#' - http://lightgbm.readthedocs.io/en/latest/Parameters-Tuning.html
#'
#' @note see also https://www.kaggle.com/pranav84/lightgbm-fixing-unbalanced-data-lb-0-9680/code
#'  
common.modeling.getHyperparams <- function(.size = 100L,
                                           .learning_rate = .01,
                                           .max_depth = -1L, 
                                           .max_bin = 255L,
                                           .num_leaves = 31L,
                                           .min_data_in_leaf = 20L, 
                                           .min_sum_hessian_in_leaf = 1e-3,
                                           .feature_fraction = 1,
                                           .bagging_fraction = 1,
                                           .bagging_freq = 0,
                                           .lambda_l1 = 0,
                                           .lambda_l2 = 0,
                                           .min_split_gain = 0,
                                           .scale_pos_weight = 1) {
  
  stopifnot(
    is.numeric(.size) && .size > 0
  )
  
  expand.grid(learning_rate = .learning_rate,
              max_depth = .max_depth,
              max_bin = .max_bin,
              num_leaves = .num_leaves,
              min_data_in_leaf = .min_data_in_leaf,
              min_sum_hessian_in_leaf = .min_sum_hessian_in_leaf,
              feature_fraction = .feature_fraction,
              bagging_fraction = .bagging_fraction,
              bagging_freq = .bagging_freq,
              lambda_l1 = .lambda_l1,
              lambda_l2 = .lambda_l2,
              min_split_gain = .min_split_gain,
              scale_pos_weight = .scale_pos_weight) %>% 
    sample_n(
      .size, replace = F
    )
}



#' 
#'
#' @param .train 
#' @param .test 
#' @param .params 
#' @param .nrounds 
#' @param .num_threads 
#'
common.modeling.train <- function(.train, .test, .params, .nrounds = 100L, .num_threads = parallel::detectCores(logical = T)) {
  require(lightgbm)
  stopifnot(
    is.list(.params),
    is.numeric(.nrounds) && .nrounds > 0
  )
  
  
  if (is.null(.test)) {
    model <- lightgbm(data = .train,
                      params = .params,
                      metric = "auc",
                      boosting = "dart", 
                      objective = "binary",
                      is_unbalance = F,
                      nrounds = .nrounds,
                      early_stopping_rounds = .nrounds/5,
                      num_threads = .num_threads,
                      save_binary = F,
                      verbose = 1)
  } else {
    model <- lgb.train(data = .train,
                       valids = list(Train = .train, Test = .test),
                       params = .params,
                       metric = "auc",
                       boosting = "gbdt",
                       objective = "binary",
                       is_unbalance = F,
                       nrounds = .nrounds,
                       early_stopping_rounds = .nrounds/2,
                       num_threads = .num_threads,
                       save_binary = F,
                       verbose = 1)
  }
  
  
  model
}



#' 
#'
#' @param .train 
#' @param .test 
#' @param .gridSearch 
#' @param .nrounds 
#' @param .num_threads 
#'
common.modeling.selectHyperparams <- function(.train, .test, .gridSearch, .nrounds = 10L, .num_threads = parallel::detectCores(logical = T)) {
  require(dplyr)
  stopifnot(
    is.data.frame(.gridSearch)
  )
  
  
  trainPerf <- numeric(nrow(.gridSearch))
  testPerf <- numeric(nrow(.gridSearch))
  
  for (i in 1:nrow(.gridSearch)) {
    model <- common.modeling.train(.train, .test, .gridSearch[i, ] %>% as.list, .nrounds, .num_threads)
    
    trainPerf[i] <- max(as.numeric(model$record_evals$Train$auc$eval))
    testPerf[i] <- max(as.numeric(model$record_evals$Test$auc$eval))
  }
  
  .gridSearch %>% 
    cbind(list(
      AUC_train = trainPerf,
      AUC_test = testPerf
    )) %>% 
    mutate(
      AUC_diff = abs(AUC_train - AUC_test)
    ) %>% 
    arrange(-AUC_test)
}



### Evaluate funs ----

#' Evaluate model
#' 
#' @param .threshold 
#' @param .penalties 
#' @param dt Scored dataset
#' 
common.modeling.evaluateModel <- function(dt, .threshold = .5, .penalties = c(1, 1)) {
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(PRROC)
  
  stopifnot(
    is.data.frame(dt) && nrow(dt) > 0,
    !anyNA(dt),
    all(c("Score", "Label") %in% names(dt)),
    is.numeric(.threshold) && .threshold > 0 && .threshold < 1,
    is.numeric(.penalties) && length(.penalties == 2)
  )
  
  
  getModelMetrix <- function(.dt) {
    stopifnot(is.grouped_df(.dt))
    
    confMatrix <- .dt %>% 
      spread(
        Actual, W, fill = 0
      ) %>%
      arrange(-Predicted)
    
    mConfMatrix <- as.matrix(confMatrix %>% select(-Predicted))
    rownames(mConfMatrix) <- paste("Predicted", confMatrix$Predicted, sep = "_")
    colnames(mConfMatrix) <- paste("Actual", names(confMatrix)[-1], sep = "_")
    
    
    FP <- mConfMatrix[1, ][[1]] * .penalties[1] # type I error
    TP <- mConfMatrix[1, ][[2]]
    TN <- mConfMatrix[2, ][[1]]
    FN <- mConfMatrix[2, ][[2]] * .penalties[2] # type II error
    
    recall <- TP/(TP + FN)
    prec <- TP/(TP + FP)
    
    
    list(
      Accuracy = (TP + TN)/(TP + TN + FP + FN) ,
      Recall = TP/(TP + FN),
      Precision = TP/(TP + FP),
      F1 = 2 * (recall * prec)/(recall + prec),
      TP = TP, FP = FP, TN = TN, FN = FN
    )
  }
  
  
  dt.grouped <- dt %>%
    mutate(
      Actual = Label,
      Predicted = if_else(Score >= .threshold, 1L, 0L)
    ) %>%
    group_by(Actual, Predicted)
  
  modelMetrix <- list(
    N = getModelMetrix(dt.grouped %>% summarise(W = n()))
  )
  
  scores <- unique(dt$Label) %>% 
    map(
      ~ dt %>% filter(Label == .x) %>% select(Score) %>% as_vector
    ) %>% 
    set_names(
      as.character(unique(dt$Label))
    )
  
  list(
    NRows = nrow(dt),
    ROC_AUC = roc.curve(scores.class0 = scores[["1"]], scores.class1 = scores[["0"]], curve = F)$auc,
    PR_ROC = pr.curve(scores.class0 = scores[["1"]], scores.class1 = scores[["0"]], curve = F)$auc.integral,
    Threshold = .threshold,
    PerfMetrix = modelMetrix
  )
}



#' Save artifacts
#'
#' @param .model 
#' @param .test 
#' @param .metrics 
#' @param .grid 
#' @param .outputDir 
#' @param modelName 
#' 
common.modeling.saveArtifacts <- function(modelName, .model, .test, .metrics, .grid, .outputDir) {
  require(jsonlite)
  require(lightgbm)
  
  stopifnot(
    is.character(modelName),
    is.data.frame(.test),
    is.list(.metrics),
    is.data.frame(.grid),
    is.character(.outputDir)
  )
  
  # prepare
  dir <- sprintf("%s/%s", .outputDir, strftime(Sys.time(), "%Y%m%d_%H%M%S"))
  if (!dir.exists(dir)) dir.create(dir)
  
  # save
  cat(toJSON(.metrics, pretty = T, simplifyVector = T),
      file = sprintf("%s/report.json", dir))
  
  saveRDS(.grid, sprintf("%s/grid.rds", dir))
  
  lgb.save(.model, sprintf("%s/lgbm.model", dir))
  
  write.csv(
    .test, 
    file = sprintf("%s/dataset.csv", dir), 
    quote = F, fileEncoding = "UTF8", na = "", row.names = F
  )
  
  # notify
  write(
    sprintf("Artifacts saved to %s", dir), stdout()
  )
}





