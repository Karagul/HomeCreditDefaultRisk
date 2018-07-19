

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
prevLoan.filter <- function(dt, .minObservationNumber = 100L) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.numeric(.minObservationNumber)
  )
  
  
  dt %>% 
    ## prepare to filter
    mutate_if(
      is.character,
      funs(if_else(!(is.na(.) | . %in% c("XAP", "XNA")), 
                   str_replace_all(str_to_lower(.), "\\W", "_"),
                   NA_character_))
    ) %>% 
    mutate_at(
      vars(starts_with("Name")),
      funs(if_else(!is.na(.), ., "-1"))
    ) %>% 
    ## filter rare cases
    group_by(
      NameContractType, NameContractStatus
    ) %>% 
    filter(
      n() >= .minObservationNumber
    ) %>% 
    ungroup() %>% 
    ## another filters
    filter(
      RateDownPayment >= 0
    )
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
    mutate(
      FlagLastApplPerContract = if_else(FlagLastApplPerContract == "Y", 1L, 0L),
      FlagCashThroughBank = if_else(NamePaymentType == "cash_through_the_bank", 1L, 0L, -1L)
      # task: long runnning convert operation
      # WeekdayApprProcessStart = wdays[WeekdayApprProcessStart],
      # NameYieldGroup = if_else(!is.na(NameYieldGroup), interestRates[NameYieldGroup], 0L) 
    ) %>% 
    select(
      -NamePaymentType, # use in FlagCashThroughBank
      -FlagLastApplPerContract, -NflagLastApplInDay # low variance
    ) %>% 
    ## numeric features preprocessing
    mutate_at(
      vars(starts_with("Days")),
      funs(if_else(. != 365243, as.integer(.), NA_integer_))
    ) %>% 
   mutate(
     SellerplaceArea = if_else(SellerplaceArea != -1, SellerplaceArea, NA_integer_)
   ) %>% 
   ## add features
   mutate(
     CreditDuration = AmtCredit/AmtAnnuity,
     CreditPercent = (AmtCredit - AmtGoodsPrice) / AmtCredit / CreditDuration * 12,
     AmtApplication_AmtCredit_ratio = AmtCredit/AmtApplication,
     AmtDownPayment_AmtAnnuity_ratio = AmtDownPayment/AmtCredit
   )
}




#' 
#'
#' @param dt 
#' @param .fillNA 
#' @param installmentsPayments 
#'
prevLoan.getHistoryStats <- function(dt, installmentsPayments, .fillNA = NA_real_) {
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  stopifnot(
    is.data.frame(dt),
    is.data.frame(installmentsPayments),
    is.numeric(.fillNA)
  )
  
  keyField <- "SkIdCurr"
  
  
  ## 1
  dt <- dt %>% 
    # add installments payments info
    inner_join(installmentsPayments, by = "SkIdPrev") %>% 
    # order by decision about previous application made
    arrange(DaysDecision)
  
  
  ## 2
  values <- setdiff(names(dt %>% select_if(is.numeric)),
                    c(keyField, "SkIdPrev"))
  
  dt %>% 
    common.fe.calcStatsByGroups(.,
                                keyField, 
                                list(".", "NameContractType", "NameContractStatus", "NameProductType", "NamePortfolio", 
                                     "NameClientType", "ProductCombination", "FlagCashThroughBank", "NameYieldGroup"),
                                values,
                                .fillNA = NA_real_, .drop = T) %>% 
    # fill NAs if necessary
    mutate_if(
      is_double,
      funs(replace_na(., .fillNA))
    )
}



#'
#'
#' @param dt 
#'
prevLoan.featureSelection <- function(dt, .minSD = .01, .minNA = .01) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.numeric(.minSD),
    is.numeric(.minNA)
  )
  
  
  keyField <- "SkIdCurr"
  
  dt %>%
    ## remove redundant fields by mask
    select(
      -matches("_(first)")
    ) %>% 
    select(
      -matches("_min_([[:alnum:]]_)?max"),
      -matches("_max_([[:alnum:]]_)?min"),
      -matches("_length_([[:alnum:]]_)+length"),
      -matches("_(min|median|max)_([[:alnum:]]_)?length")
    ) %>% 
    ## remove redundant fields by stats
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    ) %>%
    select(
      -one_of(common.fe.findCorrelatedCols(., .threshold = .9, .fillNA = 0, .extraFields = keyField))
    )
}


