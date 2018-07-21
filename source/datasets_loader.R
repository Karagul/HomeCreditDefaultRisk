

#'
#' Datasets laoders
#'


source("common_fe.R")



#' Load datasets description
#'
loader.getDatasetsDesc <- function(.config = job$Config) {
  fread(sprintf("%s/HomeCredit_columns_description.csv", job$Config$DataDir))
}



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



#' Load payment history for previous loans at bank
#'
loader.installments_payments_stats <- function(.config = job$Config) {
  
  source("installments_payment.R")
  
  
  dt <- installments_payment.load(.config) %>%
    installments_payment.clean %>%
    installments_payment.preprocessing %>% 
    installments_payment.getHistoryStats
  
  names(dt)[-1] <- paste0("instlmt__", names(dt)[-1])
  
  dt
}



#' Load monthly data about previous credit cards clients have had with bank
#' 
loader.credit_card_balance_stats <- function(.config = job$Config) {
  
  source("credit_card_balance.R")
  
  
  dt <- credit_card_balance.load(.config)
  metadata <- credit_card_balance.getMetadata(dt)
  
  dt <- dt %>%
    credit_card_balance.clean %>% 
    credit_card_balance.convert(., metadata) %>% 
    credit_card_balance.preprocessing %>% 
    credit_card_balance.getHistoryStats
  
  names(dt)[-1] <- paste0("ccb__", names(dt)[-1])
  
  dt
}



#' Load monthly data about previous point of sale or cash loans clients have had with bank
#' 
loader.pos_cash_balance_stats <- function(.config = job$Config) {
  
  source("pos_cash_balance.R")
  
  
  dt <- pos_cash_balance.load(.config)
  metadata <- pos_cash_balance.getMetadata(dt)
  
  dt <- dt %>% 
    pos_cash_balance.clean(., metadata) %>% 
    pos_cash_balance.convert(., metadata) %>% 
    pos_cash_balance.getHistoryStats
  
  names(dt)[-1] <- paste0("pcb__", names(dt)[-1])
  
  dt
}



#' Load previous loans application at bank
#'
loader.prevLoans <- function(.config = job$Config) {
  
  source("previous_loan.R")
  
  
  credit_card_balances <- loader.credit_card_balance_stats()
  installments_payments <- loader.installments_payments_stats()
  pos_cash_balances <- loader.pos_cash_balance_stats()
  
  
  dt <- prevLoan.load(.config) %>% 
    prevLoan.filter %>% 
    prevLoan.preprocessing %>% 
    prevLoan.getHistoryStats(., installments_payments, credit_card_balances, pos_cash_balances) %>% 
    prevLoan.featureSelection
  
  
  names(dt)[-1] <- paste0("prev__", names(dt)[-1])
  
  dt
}


