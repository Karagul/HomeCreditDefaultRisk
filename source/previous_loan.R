

#'
#'
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
  
  
 prevLoans %>% 
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
prevLoan.getHistory <- function(dt) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt)
  )
  
  
  groupByFields <- c("SkIdCurr", "NameContractType", "NameContractStatus")
  
  dt.g <- dt %>% 
    select(
      # group by fields
      one_of(groupByFields),
      # calc stats for fields
      starts_with("Amt"), starts_with("Rate"), starts_with("Days")
    ) %>% 
    mutate(
      CreditDuration = AmtCredit/AmtAnnuity,
      CreditPercent = (AmtCredit - AmtGoodsPrice) / AmtCredit / CreditDuration * 12,
      AmtApplication_AmtCredit_ratio = AmtCredit/AmtApplication,
      AmtDownPayment_AmtAnnuity_ratio = AmtDownPayment/AmtCredit,
      
      
      RateDownPayment_w = if_else(!is.na(RateDownPayment), 1L, 0L),
      RateInterestPrivileged_w = if_else(!is.na(RateInterestPrivileged), 4L, 0L),
      RateInterestPrimary_w = if_else(!is.na(RateInterestPrimary), 2L, 0L),
      
      RateInterest_w = RateDownPayment_w + RateInterestPrivileged_w + RateInterestPrimary_w,
      RateInterest_mean = if_else(RateInterest_w == 7,
                                  (RateDownPayment + RateInterestPrivileged +  RateInterestPrimary)/3,
                                  if_else(RateInterest_w == 6,
                                          (RateInterestPrivileged + RateInterestPrimary)/2,
                                          if_else(RateInterest_w == 5,
                                                  (RateDownPayment + RateInterestPrivileged)/2,
                                                  if_else(RateInterest_w == 3,
                                                          (RateDownPayment + RateInterestPrimary)/2,
                                                          if_else(RateInterest_w == 4,
                                                                  RateInterestPrivileged,
                                                                  if_else(RateInterest_w == 2,
                                                                          RateInterestPrimary,
                                                                          if_else(RateInterest_w == 1,
                                                                                  RateDownPayment,
                                                                                  NA_real_)))))))
    ) %>% 
    select(
      -AmtApplication, -AmtDownPayment, -AmtGoodsPrice,
      -RateDownPayment_w, -RateInterestPrivileged_w, -RateInterestPrimary_w, -RateInterestPrivileged, - RateInterestPrimary,
      -DaysDecision, -DaysFirstDrawing
    ) %>% 
    group_by_at(groupByFields)
  
  
  
  dt.g %>% 
    summarise(
      N = n()
    ) %>% 
    inner_join(
      dt.g %>% summarise_if(is.numeric, funs("min" = min(., na.rm = T))),
      by = groupByFields
    )  %>% 
    inner_join(
      dt.g %>% summarise_if(is.numeric, funs("median" = median(., na.rm = T))),
      by = groupByFields
    ) %>% 
    inner_join(
      dt.g %>% summarise_if(is.numeric, funs("max" = max(., na.rm = T))),
      by = groupByFields
    ) %>% 
    # inner_join(
    #   dt.g %>% summarise_if(is.numeric, funs("sum" = sum(., na.rm = T))),
    #   by = groupByFields
    # ) %>% 
    # inner_join(
    #   dt.g %>% summarise_if(is.numeric, funs("mad" = mad(., na.rm = T))),
    #   by = groupByFields
    # ) %>% 
    mutate_if(
      is.numeric, 
      funs(if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))
    ) %>% 
    ungroup()
}




#' 
#'
#' @param dt 
#' @param .minObservationNumber 
#' @param .minSD 
#' @param .minNotNA 
#'
prevLoan.getHistoryStats <- function(dt, .minObservationNumber = 100, .minSD = .01, .minNotNA = .01, .replaceNA = NA_real_) {
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



