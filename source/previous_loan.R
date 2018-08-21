

#'
#' Previous loans application at bank
#'



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
  stopifnot(is.data.frame(dt))


 dt %>%
   ## character features preprocessing
   # convert char to bit
   mutate(
     FlagLastApplPerContract = if_else(FlagLastApplPerContract == "Y", 1L, 0L),
     FlagCashThroughBank = if_else(NamePaymentType == "cash_through_the_bank", 1L, 0L, -1L)
   ) %>% 
   inner_join(
      data.frame(
        NameYieldGroup = c("-1", "low_action", "low_normal", "middle", "high"),
        NameYieldGroup_rate = c(0, 1, 2, 4, 8)
      ),  by = "NameYieldGroup"
    ) %>% 
    select(
      -NamePaymentType, -NameYieldGroup,  # use in FlagCashThroughBank, NameYieldGroup_rate
      -FlagLastApplPerContract, -NflagLastApplInDay # low variance
    ) %>% 
   ## numeric features preprocessing
   mutate(
     SellerplaceArea = if_else(SellerplaceArea != -1, SellerplaceArea, NA_integer_)
   ) %>% 
   # credit info features
   mutate(
     CreditDuration = AmtCredit/AmtAnnuity,
     CreditPercent = (AmtCredit - AmtGoodsPrice) / AmtCredit / CreditDuration * 12,
     AmtApplication_AmtCredit_ratio = AmtCredit/AmtApplication,
     AmtDownPayment_AmtAnnuity_ratio = AmtDownPayment/AmtCredit
   ) %>% 
   # days features
   mutate_at(
     vars(starts_with("Days")),
     funs(if_else(. != 365243, as.integer(.), NA_integer_))
   ) %>% 
   mutate(
     DaysDecision_DaysTermination_ratio = DaysTermination/DaysDecision,
     DaysDecision_DaysFirstDrawing_ratio = DaysFirstDrawing/DaysDecision,
     DaysFirstDrawing_DaysFirstDue_ratio = DaysFirstDue/DaysFirstDrawing,
     DaysFirstDue_DaysLastDue_ratio = DaysLastDue/DaysFirstDue,
     DaysLastDue1stVersion_DaysLastDue_ratio = if_else(DaysLastDue1stVersion != 0, DaysLastDue/DaysLastDue1stVersion, 0),
     DaysLastDue1stVersion_flag = if_else(DaysLastDue1stVersion < 0, 0L, 1L, -1L)
   ) %>% 
   select(
     -DaysTermination, -DaysFirstDrawing, -DaysFirstDue, -DaysLastDue, -DaysLastDue1stVersion
   )
}



#' 
#'
#' @param dt 
#' @param .fillNA 
#' @param installments_payments 
#' @param credit_card_balances 
#' @param pos_cash_balances 
#'
prevLoan.getHistoryStats <- function(dt, 
                                     installments_payments, credit_card_balances, pos_cash_balances,
                                     .fillNA = NA_real_) {
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  keyField <- "SkIdPrev"
  groupByField <- "SkIdCurr"
  
  stopifnot(
    is.data.frame(dt),
    is.data.frame(installments_payments),
    is.data.frame(credit_card_balances),
    is.data.frame(pos_cash_balances),
    intersect(names(credit_card_balances), names(installments_payments)) == keyField,
    intersect(names(installments_payments), names(pos_cash_balances)) == keyField,
    intersect(names(pos_cash_balances), names(credit_card_balances)) == keyField,
    is.numeric(.fillNA)
  )
  
  
  ## 1
  dt <- dt %>% 
    # add installments payments info
    inner_join(installments_payments, by = keyField) %>% 
    inner_join(credit_card_balances, by = keyField) %>% 
    inner_join(installments_payments, by = keyField) %>% 
    # order by decision about previous application made
    arrange(DaysDecision)
  
  
  ## 2
  values <- setdiff(names(dt %>% select_if(is.numeric)),
                    c(keyField, groupByField, "HourApprProcessStart", "SellerplaceArea"))
  
  dt %>% 
    common.fe.calcStatsByGroups(.,
                                groupByField, 
                                list(".", "NameContractType", "NameContractStatus", "NamePortfolio", "NameClientType", "ProductCombination"),
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
prevLoan.featureSelection <- function(dt, .minSD = .01, .minNA = .2) {
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
      -matches("_(first)"),
      -matches("_min_([[:alnum:]]+_)?max"),
      -matches("_max_([[:alnum:]]+_)?min"),
      -matches("_(min|median|mean|max|sd|mad|length)_([[:alnum:]]+_)?length")
    ) %>% 
    ## remove redundant fields by stats
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    ) %>%
    select(
      -one_of(common.fe.findCorrelatedCols(., .threshold = .95, .extraFields = keyField))
    )
}


