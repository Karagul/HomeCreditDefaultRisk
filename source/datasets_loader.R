

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
          loan.missingValuesProcessing(., loan.metadata)
    )
}



#' Load previous loans info from credit bureaus
#'
loader.bureau <- function(.config = job$Config) {
  
  source("bureau.R")
  
  bureau.history <- bureau.load(.config) %>% bureau.getHistory
  # saveRDS(bureau.history, "cache/bureau.history.rds")
  
  bureau.history.stats <- bureau.getHistoryStats(bureau.history, .replaceNA = 0)
  stopifnot(
    !anyNA(bureau.history.stats)
  )
  
  bureau.history.stats
}



#' Load previous loans application at bank
#'
loader.prevLoans <- function(.config = job$Config) {
  
  source("previous_loan.R")
  
  prevLoans <- prevLoan.load(.config) %>% 
    prevLoan.filter %>% 
    prevLoan.preprocessing
  
  prevLoans.history <- prevLoan.getHistory(prevLoans)
  #saveRDS(prevLoans.history, "cache/prevLoans.history.rds")
  
  prevLoans.history.stats <- prevLoan.getHistoryStats(prevLoans.history, .replaceNA = 0)
  stopifnot(
    nrow(prevLoans.history.stats) > 0, 
    !anyNA(prevLoans.history.stats)
  )
  
  prevLoans.history.stats
}



#' Load payment history for previous loans at bank
#'
loader.installments_payments_stats <- function(.config = job$Config, .calcStatsBy = c("SkIdPrev")) {
  
  source("installments_payment.R")
  

  dt <- installments_payment.load(.config)
  
  metadata <- installments_payment.getMetadata(dt)
  
  dt <- dt %>%
    installments_payment.clean(., metadata) %>%
    installments_payment.convert(., metadata)
  
  dt.stats <- common.modeling.calcDescStats(dt %>% 
                                              mutate(
                                                DaysInstalment_DaysEntryPayment_diff = DaysInstalment - DaysEntryPayment,
                                                AmtInstalment_AmtPayment_diff = AmtInstalment - AmtPayment
                                                ) %>% 
                                              select(-SkIdCurr),
                                            .calcStatsBy)
  
  glimpse(dt.stats)
  
  
  dt.stats.w <- installments_payment.toWideTable(dt.stats, .fillNA = 0)%>% 
    inner_join(
      dt %>% distinct(SkIdPrev, SkIdCurr, .keep_all = F),
      by = "SkIdPrev"
    )
  
  describe(dt.stats.w)
  
  loader._flatternStats(dt.stats.w)
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

