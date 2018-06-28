
source("previous_loan.R")
source("bureau.R")
source("installments_payment.R")
source("credit_card_balance.R")
source("pos_cash_balance.R")



#' 
#'
#' 
loader.bureau <- function() {
  bureau.history <- bureau.load(job$Config) %>% bureau.getHistory # readRDS("cache/bureau.history.rds")
  saveRDS(bureau.history, "cache/bureau.history.rds")
  
  bureau.history.stats <- bureau.getHistoryStats(bureau.history, .replaceNA = 0)
  stopifnot(
    !anyNA(bureau.history.stats)
  )
  
  bureau.history.stats
}



#'
#'
#' 

loader.prevLoans <- function() {
  
  prevLoans <- prevLoan.load(job$Config) %>% 
    prevLoan.filter %>% 
    prevLoan.preprocessing
  
  prevLoans.history <- prevLoan.getHistory(prevLoans)
  saveRDS(prevLoans.history, "cache/prevLoans.history.rds")
  
  prevLoans.history.stats <- prevLoan.getHistoryStats(prevLoans.history, .replaceNA = 0)
  stopifnot(
    !anyNA(prevLoans.history.stats)
  )
  
  prevLoans.history.stats
}



#' 
#'
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



#'
#'
#' 
loader.installments_payments_stats <- function() {

  installments_payments <- installments_payment.load(job$Config)
  
  installments_payments.metadata <- installments_payment.getMetadata(installments_payments)
  
  installments_payments <- installments_payments %>%
    installments_payment.clean(., installments_payments.metadata) %>% 
    installments_payment.convert(., installments_payments.metadata)
  
  installments_payments.stats <- common.modeling.calcDescStats(installments_payments %>% 
                                                                 mutate(
                                                                   DaysInstalment_DaysEntryPayment_diff = DaysInstalment - DaysEntryPayment,
                                                                   AmtInstalment_AmtPayment_diff = AmtInstalment - AmtPayment
                                                                 ) %>% 
                                                                 select(-SkIdCurr),
                                                               c("SkIdPrev"))
  
  glimpse(installments_payments.stats)
  
  
  installments_payments.wstats <- installments_payment.toWideTable(installments_payments.stats, .fillNA = 0)%>% 
    inner_join(
      installments_payments %>% distinct(SkIdPrev, SkIdCurr, .keep_all = F),
      by = "SkIdPrev"
    )
  
  describe(installments_payments.wstats)
  
  loader._flatternStats(installments_payments.wstats)
}



#'
#'
#' 
loader.credit_card_balance_stats <- function() {
  
  credit_card_balances <- credit_card_balance.load(job$Config)
  
  credit_card_balances.metadata <- credit_card_balance.getMetadata(credit_card_balances)
  
  credit_card_balances <- credit_card_balances %>%
    credit_card_balance.clean(., credit_card_balances.metadata) %>% 
    credit_card_balance.convert(., credit_card_balances.metadata)
  
  credit_card_balances.stats <- common.modeling.calcDescStats(credit_card_balances %>% 
                                                                select(-SkIdCurr), 
                                                              c("SkIdPrev", "NameContractStatus")) %>% 
    mutate(
      MonthsBalance_range = MonthsBalance_max - MonthsBalance_min
    )
  
  glimpse(credit_card_balances.stats)
  
  credit_card_balances.wstats <- credit_card_balance.toWideTable(credit_card_balances.stats, .fillNA = 0) %>% 
    inner_join(
      credit_card_balances %>% distinct(SkIdPrev, SkIdCurr, .keep_all = F),
      by = "SkIdPrev"
    )
  
  describe(pos_cash_balances.wstats)
  
  
  loader._flatternStats(credit_card_balances.wstats)
}



#'
#'
#' 
loader.pos_cash_balance_stats <- function() {
  pos_cash_balances <- pos_cash_balance.load(job$Config)
  
  pos_cash_balances.metadata <- pos_cash_balance.getMetadata(pos_cash_balances)
  
  pos_cash_balances <- pos_cash_balances %>% 
    pos_cash_balance.clean(., pos_cash_balances.metadata) %>% 
    pos_cash_balance.convert(., pos_cash_balances.metadata)
  
  pos_cash_balances.stats <- common.modeling.calcDescStats(pos_cash_balances %>% 
                                                             mutate(CntInstalment_CntInstalmentFuture_diff = CntInstalment - CntInstalmentFuture) %>% 
                                                             select(-SkIdCurr),
                                                           c("SkIdPrev", "NameContractStatus"))
  
  glimpse(pos_cash_balances.stats)
  
  
  
  pos_cash_balances.wstats <- pos_cash_balance.toWideTable(pos_cash_balances.stats, .fillNA = 0) %>% 
    inner_join(
      pos_cash_balances %>% distinct(SkIdPrev, SkIdCurr, .keep_all = F),
      by = "SkIdPrev"
    )
  
  describe(pos_cash_balances.wstats)
  
  
  loader._flatternStats(pos_cash_balances.wstats)
}


