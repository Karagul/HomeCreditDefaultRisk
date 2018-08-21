

#' 
#' Default model-specific functions
#' 



#' 
#'
#' @param .datasets 
#' @param .metadata 
#' 
removeRedundantCols <- function(.datasets, .metadata) {
  
  cols <- setdiff(common.fe.findRedundantCols(.datasets$Test), unlist(.metadata))
  
  stopifnot(
    !any(setdiff(cols, names(.datasets$Test)))
  )
  
  write(sprintf("Deleting cols: %s", paste(cols, collapse = ", ")), stdout())
  
  .datasets %>% 
    map(~ .x %>% select(-one_of(cols)))
}



#' 
#'
#' @param .datasets 
#' @param .metadata 
#' 
splitDataset <- function(.datasets, .metadata) {
  stopifnot(
    is.list(.datasets) && length(.datasets) == 2,
    is.list(.metadata)
  )
  
  
  sDatasets <- common.modeling.splitDataset(.datasets$Train, .metadata$Key)
  .datasets$Train <- sDatasets$Train
  .datasets$Valid <- sDatasets$Valid
  
  stopifnot(
    length(.datasets) == 3,
    nrow(.datasets$Train) > nrow(.datasets$Valid),
    nrow(.datasets$Valid) > nrow(.datasets$Test),
    nrow(.datasets$Test) > 0
  )
  
  .datasets
}



#' 
#'
#' @param .datasets 
#' @param .metadata 
#'
encodingFeatures <- function(.datasets, .metadata) {
  require(dplyr)
  require(purrr)
  
  stopifnot(
    is.list(.datasets) && length(.datasets) > 1,
    is.list(.metadata)
  )
  
  
  .metadata$Features$FactorSLE <- c(.metadata$Features$FactorSLE, c("EmergencystateMode"))
  
  ## calc encoders
  encoders.OH <- .metadata$Features$FactorOHE %>% 
    map(~ common.modeling.getOneHotEncoder(.x, .datasets$Train))
  
  encoders.SLE <- .metadata$Features$FactorSLE %>% 
    map(~ common.modeling.smoothedLikelihoodEncoding(.x, .datasets$Train))
  
  
  ## apply encoders
  .datasets <- .datasets %>%
    map(~ common.modeling.applyEncoders(.x, encoders.OH, .metadata$Features$FactorOHE)) %>%
    map(~ common.modeling.applyEncoders(.x, encoders.SLE, .metadata$Features$FactorSLE))
  
  stopifnot(
    length(.datasets) > 1,
    nrow(.datasets$Train) > nrow(.datasets$Test),
    nrow(.datasets$Test) > 0
  )
  
  
  ## replace missing values
  # for SLE-encoded features
  factorSLE.pattern <- sprintf("^(%s)+_SL", paste(.metadata$Features$FactorSLE, collapse = "|"))
  
  .datasets <- .datasets %>% 
    map(
      ~ common.modeling.replaceNA(.x, .datasets$Train %>% select(matches(factorSLE.pattern)), median) # TODO: introduce .predicate
    )
  
  # for OH-encoded features # warn: transformation modify not only OHE features
  factorOHE.pattern <- sprintf("^(%s)+_", paste(.metadata$Features$FactorOHE, collapse = "|"))
  .datasets <- .datasets %>% 
    map(
      ~ .x %>% 
        mutate_at(
          vars(matches(factorOHE.pattern)), 
          funs(ifelse(!is.na(.), ., 0)))
    )
  
  stopifnot(
    all(
      list(
        .metadata$Features$FactorSLE,
        .metadata$Features$FactorOHE
      ) %>% 
        map2(
          list(factorSLE.pattern, factorOHE.pattern),
          ~ if (is_empty(.x)) {
            .datasets %>% map_lgl(~ T)
          } else {
            .datasets %>% 
              map_lgl(
                function(..x) nrow(..x %>% 
                                     select(matches(.y)) %>% 
                                     filter_all(any_vars(is.na(.)))) == 0
              )
          }
        ) %>% 
        as_vector
    )
  )
  
  
  .datasets
}




#' 
#'
#' @param .datasets 
#' @param .metadata 
#'
#' TODO: replace to common.modeling.replaceNA
#' 
replaceMissingValues <- function(.datasets, .metadata, .fields, .predicate) {
  stopifnot(
    is.list(.datasets) && length(.datasets) > 1,
    is.list(.metadata),
    is.character(.fields),
    is.function(.predicate)
  )
  
  
  fieldNames <- names(.datasets$Train %>% select(matches(.fields)))
  
  if (is_empty(fieldNames)) return(.datasets)
  
  
  write(sprintf("Replace missing values for: %s", paste(fieldNames, collapse = ", ")), stdout())
  
  dt.stats <- fieldNames %>% 
    map(
      ~ .predicate(.datasets$Train[[.x]], na.rm = T, trim = .05)
    ) %>%
    set_names(., fieldNames)
  
  
  .datasets %>% 
    map(
      function(.x) {
        
        for (i in 1:length(dt.stats)) {
          .x <- .x %>% 
            mutate_at(
              names(dt.stats[i]),
              funs(replace_na(., dt.stats[[i]] ))
            )
        }
        
        .x
      }
    )
}

