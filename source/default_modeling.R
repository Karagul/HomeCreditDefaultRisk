

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
  
  
  sDatasets <- common.modeling.splitDataset(.datasets$Train, "SkIdCurr") # TODO: change on .metadata$KeyField
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
  stopifnot(
    is.list(.datasets) && length(.datasets) == 3,
    is.list(.metadata)
  )
  
  
  ## calc encoders
  encoders.OH <- .metadata$Features$FactorOHE %>% 
    map(~ common.modeling.getOneHotEncoder(.x, .datasets$Train))
  
  .metadata$Features$FactorSLE <- c(.metadata$Features$FactorSLE, c("EmergencystateMode"))
  
  encoders.SLE <- .metadata$Features$FactorSLE %>% 
    map(~ common.modeling.smoothedLikelihoodEncoding(.x, .datasets$Train))
  
  
  ## apply encoders
  .datasets <- .datasets %>%
    map(~ common.modeling.applyEncoders(.x, encoders.OH, .metadata$Features$FactorOHE)) %>%
    map(~ common.modeling.applyEncoders(.x, encoders.SLE, .metadata$Features$FactorSLE))
  
  stopifnot(
    length(.datasets) == 3,
    nrow(.datasets$Train) > nrow(.datasets$Valid),
    nrow(.datasets$Valid) > nrow(.datasets$Test),
    nrow(.datasets$Test) > 0
  )
  
  
  ## replace missing values
  # for SLE-encoded features
  factorSLE.pattern <- sprintf("^(%s)+_SL", paste(.metadata$Features$FactorSLE, collapse = "|"))
  
  .datasets <- .datasets %>% 
    map(
      ~ common.modeling.replaceNA(.x, .datasets$Train %>% select(matches(factorSLE.pattern)), median)
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
replaceMissingValues <- function(.datasets, .metadata) {
  stopifnot(
    is.list(.datasets) && length(.datasets) == 3,
    is.list(.metadata)
  )
  
  
  fieldNames <- names(.datasets$Train %>% 
                        select(matches("(Avg|Mode|Medi)$")))
  
  write(sprintf("Replace missing values for: %s", paste(fieldNames, collapse = ", ")), stdout())
  
  apartments.stats <- fieldNames %>% 
    map(
      ~ median(.datasets$Train[[.x]], trim = .05, na.rm = T)
    ) %>%
    set_names(., fieldNames)
  
  
  .datasets %>% 
    map(
      function(.x) {
        
        for (i in 1:length(apartments.stats)) {
          .x <- .x %>% 
            mutate_at(
              names(apartments.stats[i]),
              funs(replace_na(., apartments.stats[[i]] ))
            )
        }
        
        .x
      }
    ) #%>%
  # map(
  #   ~ .x %>% 
  #     mutate_at(
  #       vars(starts_with("bureau__"), starts_with("prev__"), starts_with("...")), # TODO
  #       funs(replace_na(., 0))
  #     )
  # )
}

