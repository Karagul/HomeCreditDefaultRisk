

#'
#' Payment history for previous loans at bank
#'



#'
#'
#'
#' @param .config 
installments_payment.load <- function(.config) {
  stopifnot(is.list(.config))
  fread.csv.zipped("installments_payments.csv", .config$DataDir)
}



#' 
#'
#' @param dt 
#'
installments_payment.getMetadata <- function(dt) {
  stopifnot(is.data.frame(dt))
  
  
  key <- NULL
  label <- NULL
  extraVars <- c("SkIdCurr", "SkIdPrev")
  ignored <- NULL
  
  fetures <- list(
    Numeric = setdiff(names(dt %>% select_if(is.numeric)), c(key, label, extraVars, ignored)),
    Factors = setdiff(names(dt %>% select_if(is.character)), c(key, label, extraVars, ignored)),
    Binary = c()
  )
  
  
  meta <- list(
    Key = key,
    Features = fetures,
    Label = label,
    Ignored = ignored,
    ExtraVars = extraVars
  )
  
  stopifnot(
    all(unlist(meta, recursive = T, use.names = F) %in% names(dt)),
    !anyDuplicated(unlist(meta, recursive = T, use.names = F))
  )
  
  write(
    sprintf("Ignored fields: %s", paste(meta$Ignored, collapse = ", ")), stdout()
  )
  
  
  meta
}



#' Clear logic errors
#'
#' @param dt 
#' @param .metadata 
#' 
installments_payment.clean <- function(dt) {
  dt
}



#' 
#'
#' @param dt 
#'
installments_payment.preprocessing <- function(dt) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt)
  )
  
  dt %>% 
    mutate_at(
      vars(starts_with("Num"), starts_with("Days")),
      as.integer
    ) %>% 
    mutate_at(
      vars(starts_with("Amt")), 
      as.double
    ) %>% 
    mutate(
      DaysInstalment_DaysEntryPayment_diff = DaysInstalment - DaysEntryPayment,
      AmtInstalment_AmtPayment_diff = AmtPayment - AmtInstalment,
      AmtInstalment_AmtPayment_ratio = AmtPayment/(AmtInstalment + 1)
    ) %>% 
    select(-c(
      NumInstalmentNumber,
      DaysEntryPayment, AmtInstalment
    ))
}



#' 
#'
#' @param dt 
#' @param .fillNA 
#' @param .minObservationNumber 
#' @param .minSD 
#' @param .minNA 
#'
installments_payment.getHistoryStats <- function(dt, 
                                                 .fillNA = NA_real_,
                                                 .minObservationNumber = 100L, 
                                                 .minSD = .01, .minNA = .1) {
  
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  stopifnot(
    is.data.frame(dt),
    is.numeric(.fillNA),
    is.numeric(.minObservationNumber),
    is.numeric(.minSD),
    is.numeric(.minNA)
  )
  
  keyField <- "SkIdPrev"
  
  values <- setdiff(names(dt %>% select_if(is.numeric)),
                    c(keyField, "SkIdCurr"))
  
  dt <- dt %>% 
    arrange(DaysInstalment) %>% 
    common.fe.calcStatsByGroups(., 
                                keyField, ".", values,
                               .fillNA = NA_real_, .drop = T) %>%
    select(
      -matches("_(first|last)")
    ) %>%
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    )
}


