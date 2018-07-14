

#'
#' Previous loans application at bank
#'


# Import dependencies
source("common_fe.R")


#' 
#'
#' @param .config 
#'
prevLoan.load <- function(.config) {
  stopifnot(is.list(.config))
  fread.csv.zipped("previous_application.csv", .config$DataDir)
}



#' 
#'
#' @param dt 
#'
prevLoan.filter <- function(dt) {
  stopifnot(is.data.frame(dt))
  dt
}



#' 
#'
#' @param dt 
#'
prevLoan.preprocessing <- function(dt) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt)
  )
  
  wdays <- c(5, 2, 6, 1, 4, 3, 7); names(wdays) <- c("friday", "tuesday", "saturday", "monday", "thursday", "wednesday", "sunday")
  interestRates <- c(4:1); names(interestRates) <- c("high", "middle", "low_normal", "low_action")
  
  
 dt %>% 
    ## character features preprocessing
    mutate_if(
      is.character,
      funs(if_else(!(is.na(.) | . %in% c("XAP", "XNA")), 
                   str_replace_all(str_to_lower(.), "\\W", "_"),
                   NA_character_))
    ) %>% 
    mutate(
      FlagLastApplPerContract = if_else(FlagLastApplPerContract == "Y", 1L, 0L),
      FlagCashThroughBank = if_else(NamePaymentType == "cash_through_the_bank", 1L, 0L)
      # warn: long runnning convert operation
      # WeekdayApprProcessStart = wdays[WeekdayApprProcessStart],
      # NameYieldGroup = if_else(!is.na(NameYieldGroup), interestRates[NameYieldGroup], 0L) 
    ) %>% 
    select(
      -NamePaymentType,
      # low variance
      -FlagLastApplPerContract, -NflagLastApplInDay 
    ) %>% 
    ## numeric features preprocessing
    mutate_at(
      vars(starts_with("Days")),
      funs(if_else(. != 365243, as.integer(.), NA_integer_))
    ) %>% 
   mutate(
     SellerplaceArea = if_else(SellerplaceArea != -1, SellerplaceArea, NA_integer_)
   ) %>% 
   filter(
     RateDownPayment >= 0
   )
}




#' 
#'
#' @param dt 
#'
prevLoan.getHistoryStats <- function(dt, 
                                     .fillNA = NA_real_,
                                     .minObservationNumber = 100, 
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
  
  keyField <- "SkIdCurr"
  
  
  ## 1
  dt <- dt %>%
    # add features
    mutate(
      CreditDuration = AmtCredit/AmtAnnuity,
      CreditPercent = (AmtCredit - AmtGoodsPrice) / AmtCredit / CreditDuration * 12,
      AmtApplication_AmtCredit_ratio = AmtCredit/AmtApplication,
      AmtDownPayment_AmtAnnuity_ratio = AmtDownPayment/AmtCredit
    ) %>% 
    # fill missing categorical features
    mutate_at(
      vars(starts_with("Name")),
      funs(if_else(!is.na(.), ., "-1"))
    ) %>% 
    # filter rare cases
    group_by(
      NameContractType, NameContractStatus
    ) %>% 
    filter(
      n() >= .minObservationNumber
    ) %>% 
    ungroup() %>% 
    # order by decision about previous application made
    arrange(DaysDecision)
  
  
  ## 2
  values <- setdiff(names(dt %>% select_if(is.numeric)),
                    c(keyField, "SkIdPrev"))
  
  dt %>% 
    common.fe.calcStatsByGroups(.,
                                keyField, 
                                list(".", "NameContractType", "NameContractStatus", "NameProductType", "NamePortfolio", "NameClientType", "ProductCombination"),
                                values,
                                .fillNA = NA_real_, .drop = T)  %>% 
    # remove redundant fields by mask
    # select(
    #   -matches("_min_([[:alnum:]]_)?max"),
    # ) %>% 
    # remove redundant fields by stats
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    ) %>% 
    select(
      -one_of(common.fe.findCorrelatedCols(., .threshold = .9, .fillNA = 0, .extraFields = keyField))
    ) %>% 
    # fill NAs if nessesary
    mutate_if(
      is_double,
      funs(replace_na(., .fillNA))
    )
}



#' 
#'
#' @param dt 
#' @param .minObservationNumber 
#' @param .minSD 
#' @param .minNotNA 
#'
prevLoan._getHistoryStats <- function(dt, .minObservationNumber = 100, .minSD = .01, .minNotNA = .01, .replaceNA = NA_real_) {
  require(dplyr)
  require(data.table)
  require(purrr)
  require(psych)
  
  stopifnot(
    is.data.frame(dt),
    is.numeric(.minObservationNumber),
    is.numeric(.minSD),
    is.numeric(.minNotNA), 
    is.numeric(.replaceNA)
  )
  
  
  groupByFields <- c("SkIdCurr", "NameContractType", "NameContractStatus")
  
  dt <- dt %>% 
    group_by(NameContractType, NameContractStatus) %>% 
    filter(n() >= .minObservationNumber) %>% 
    ungroup() %>% 
    as.data.table(., key = groupByFields)
  
  dt <- melt(dt, id.var = groupByFields, variable.name = "Metrics")
  dt <- dt[, Metrics := paste("prev", NameContractType, NameContractStatus, Metrics, sep = "__")]
  
  dt <- dcast.data.table(dt, SkIdCurr ~ Metrics, value.var = "value")
  
  
  dt.desc <- describe(dt %>%
                        mutate_if(
                          is.numeric, funs(if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))
                        ))
  
  redundantFieldsIndex <- dt.desc %>% 
    filter(sd < .minSD | n/nrow(dt) < .minNotNA) %>% 
    select(vars) %>% 
    as_vector
  
  dt %>% 
    select(-one_of(names(dt)[redundantFieldsIndex])) %>% 
    mutate_if(
      is_double,
      funs(replace_na(., .replaceNA))
    )
}



