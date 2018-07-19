

#'
#' Datasets laoders
#'

source("common_modeling.R")


#' Load loan applications
#'
loader.loans <- function(.config = job$Config) {
  
  source("loan.R")
  
  loans <- loan.load(.config)
  
  loan.metadata <- loan.getMetadata(loans$Train)
  
  loans %>% 
    map(~ .x %>% 
          loan.clear(., loan.metadata) %>% 
          loan.format(., loan.metadata) %>% 
          loan.calcRequestsNumber(., .fillNA = 0) %>% 
          loan.calcDocumentNumber %>% 
          loan.calcDefaultSocialCircleNumber(., .fillNA = 0) %>% 
          loan.processingOwnership %>% 
          loan.processingIncome %>% 
          loan.processingExternalSourceScore %>% 
          loan.processingDays %>% 
          loan.missingValuesProcessing(., loan.metadata, .fillNA = -1L)
    )
}



#' Load previous loans info from credit bureaus
#'
loader.bureau <- function(.config = job$Config) {
  
  source("bureau.R")
  
  balances <- bureau.getBalancesStats(.config)
  
  dt <- bureau.load(.config) %>% 
    burea.filter %>% 
    burea.preprocessing %>% 
    bureau.getHistoryStats(., balances) %>% 
    bureau.featureSelection
  
  names(dt)[-1] <- paste0("bureau__", names(dt)[-1])
  
  dt
}



#' Load previous loans application at bank
#'
loader.prevLoans <- function(.config = job$Config) {
  
  source("previous_loan.R")
  
  installments_payments <- loader.installments_payments_stats()
  # TODO: load and join other datasets
  # pos_cash_balances <- loader.pos_cash_balance_stats()
  # credit_card_balances <- loader.credit_card_balance_stats()
  
  dt <- prevLoan.load(.config) %>% 
    prevLoan.filter %>% 
    prevLoan.preprocessing %>% 
    prevLoan.getHistoryStats(., installments_payments) %>% 
    prevLoan.featureSelection
  
  
  names(dt)[-1] <- paste0("prevLoans__", names(dt)[-1])
  
  dt
}



#' Load payment history for previous loans at bank
#'
loader.installments_payments_stats <- function(.config = job$Config) {
  
  source("installments_payment.R")
  
  installments_payment.load(.config) %>%
    installments_payment.clean %>%
    installments_payment.preprocessing %>% 
    installments_payment.getHistoryStats
}



#' Load monthly data about previous credit cards clients have had with bank
#' 
loader.credit_card_balance_stats <- function(.config = job$Config, .calcStatsBy = c("SkIdPrev", "NameContractStatus")) {
  
  source("credit_card_balance.R")
  
  
  dt <- credit_card_balance.load(.config)
  
  metadata <- credit_card_balance.getMetadata(dt)
  
  dt <- dt %>%
    credit_card_balance.clean(., metadata) %>% 
    credit_card_balance.convert(., metadata)
  
  
  dt.stats <- common.modeling.calcDescStats(dt %>% select(-SkIdCurr), .calcStatsBy) %>% 
    mutate(MonthsBalance_range = MonthsBalance_max - MonthsBalance_min)
  
  glimpse(dt.stats)
  
  
  dt.stats.w <- credit_card_balance.toWideTable(dt.stats, .fillNA = 0) %>% 
    inner_join(
      dt %>% distinct(SkIdPrev, SkIdCurr, .keep_all = F),
      by = "SkIdPrev"
    )
  
  describe(dt.stats.w)
  
  
  loader._flatternStats(dt.stats.w)
}




#' Load monthly data about previous point of sale or cash loans clients have had with bank
#' 
loader.pos_cash_balance_stats <- function(.config = job$Config, .calcStatsBy = c("SkIdPrev", "NameContractStatus")) {
  
  source("pos_cash_balance.R")
  
  
  dt <- pos_cash_balance.load(.config)
  
  metadata <- pos_cash_balance.getMetadata(dt)
  
  dt <- dt %>% 
    pos_cash_balance.clean(., metadata) %>% 
    pos_cash_balance.convert(., metadata)
  
  dt.stats <- common.modeling.calcDescStats(dt %>% 
                                              mutate(CntInstalment_CntInstalmentFuture_diff = CntInstalment - CntInstalmentFuture) %>% 
                                              select(-SkIdCurr),
                                            .calcStatsBy)
  
  glimpse(dt.stats)
  
  
  
  dt.stats.w <- pos_cash_balance.toWideTable(dt.stats, .fillNA = 0) %>% 
    inner_join(
      dt %>% distinct(SkIdPrev, SkIdCurr, .keep_all = F),
      by = "SkIdPrev"
    )
  
  describe(dt.stats.w)
  
  
  loader._flatternStats(dt.stats.w)
}



#' Flattern statistics
#' 
loader._flatternStats <- function(dt) {
  dt.g <- dt %>% group_by(SkIdCurr)
  
  result <- dt.g %>% 
    summarise(
      N = n()
    ) %>% 
    inner_join(
      dt.g %>% summarise_at(vars(ends_with("_max")), funs(max(., na.rm = T))),
      by = "SkIdCurr"
    ) %>%
    inner_join(
      dt.g %>% summarise_at(vars(ends_with("_min")), funs(min(., na.rm = T))),
      by = "SkIdCurr"
    ) %>%
    inner_join(
      dt.g %>% summarise_at(vars(ends_with("_median")), funs(median(., na.rm = T))),
      by = "SkIdCurr"
    ) %>% 
    inner_join(
      dt.g %>% summarise_at(vars(ends_with("_mad")), funs(median(., na.rm = T))),
      by = "SkIdCurr"
    )  %>% 
    mutate_if(
      is.numeric,
      funs(ifelse(is.na(.) | is.nan(.) | is.infinite(.), 0, .))
    ) %>%
    ungroup()
  
  
  stopifnot(
    nrow(result) > 0,
    !anyDuplicated(result$SkIdCurr)
  )
  
  result
}

