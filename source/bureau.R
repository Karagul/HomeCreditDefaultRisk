

#'
#'
#'



bureau.load <- function(.config) {
  require(dplyr)
  require(stringr)
  source("common_modeling.R")
  
  # calculte balances stats 
  calcBalancesStats <- function() {
    require(tidyr)
    
    bureauBalances.grouped <- fread.csv.zipped("bureau_balance.csv", .config$DataDir) %>% 
      group_by(SkIdBureau, Status) %>% 
      summarise(
        N = log(n()),
        Min = min(MonthsBalance),
        Max = max(MonthsBalance)
      )
    
    bureauBalances.grouped %>% select(-Min, -Max) %>%  spread(Status, N, sep = "_N_", fill = 0) %>% 
      inner_join(
        bureauBalances.grouped %>% select(-Min, -N) %>% spread(Status, Max, sep = "_max_", fill = 0),
        by = "SkIdBureau") %>% 
      inner_join(
        bureauBalances.grouped %>% select(-Max, -N) %>% spread(Status, Min, sep = "_min_", fill = 0),
        by = "SkIdBureau")
  }
  
  
  ## some info
  # SkIdCurr               1 1716428  278214.93  102938.56   100001.0    456255    356254  78.57  
  # SkIdBureau             2 1716428 5924434.49  532265.73  5000000.0   6843457   1843457 406.27    PK
  # CreditActive           3 1716428        NaN         NA        Inf      -Inf      -Inf     NA    isActiveCredit
  # CreditCurrency         4 1716428        NaN         NA        Inf      -Inf      -Inf     NA    isCurrency_1
  # DaysCredit             5 1716428   -1142.11     795.16    -2922.0         0      2922   0.61    
  # CreditDayOverdue       6 1716428       0.82      36.54        0.0      2792      2792   0.03    
  # DaysCreditEnddate      7 1610875     510.52    4994.22   -42060.0     31199     73259   3.93    NA
  # DaysEnddateFact        8 1082775   -1017.44     714.01   -42023.0         0     42023   0.69    NA
  # AmtCreditMaxOverdue    9  591940    3825.42  206031.61        0.0 115987185 115987185 267.79    NA + log
  # CntCreditProlong      10 1716428       0.01       0.10        0.0         9         9   0.00
  # AmtCreditSum          11 1716415  354994.59 1149811.34        0.0 585000000 585000000 877.64    NA
  # AmtCreditSumDebt      12 1458759  137085.12  677401.13 -4705600.3 170100000 174805600 560.86    NA
  # AmtCreditSumLimit     13 1124648    6229.51   45032.03  -586406.1   4705600   5292006  42.46    NA
  # AmtCreditSumOverdue   14 1716428      37.91    5937.65        0.0   3756681   3756681   4.53
  # CreditType            15 1716428        NaN         NA        Inf      -Inf      -Inf     NA
  # DaysCreditUpdate      16 1716428    -593.75     720.75   -41947.0       372     42319   0.55
  # AmtAnnuity            17  489637   15712.76  325826.95        0.0 118453424 118453424 465.64    NA
  
  fread.csv.zipped("bureau.csv", .config$DataDir) %>% 
    mutate_if(
      is.character,
      funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
    ) %>% 
    mutate(
      CreditCurrency = if_else(CreditCurrency == "currency_1", 0L, 1L),
      DaysCredit = abs(DaysCredit)
    ) %>% 
    left_join(
      calcBalancesStats(), by = "SkIdBureau"
    ) %>% 
    filter(
      CreditActive %in% c("closed", "active")
    )
}



#' 
#'
#' @param dt 
#'
bureau.getHistory <- function(dt) {
  require(dplyr)
  stopifnot(is.data.frame(dt))
  
  dt %>% 
    group_by(
      SkIdCurr, CreditType, CreditActive
    ) %>% 
    summarise(
      N = n(),
      CreditCurrencyRisk = sum(CreditCurrency, na.rm = T),
      
      Bureau_DaysCredit_max = max(DaysCredit, na.rm = T) %/% 30,
      Bureau_DaysCredit_min = max(DaysCredit, na.rm = T) %/% 30,
      Bureau_DaysCredit_range = Bureau_DaysCredit_max - Bureau_DaysCredit_min,
      
      Bureau_CreditDayOverdue_sum = sum(CreditDayOverdue, na.rm = T),
      Bureau_CreditDayOverdue_median = median(CreditDayOverdue/N, na.rm = T),
      Bureau_AmtCreditMaxOverdue_sum = sum(AmtCreditMaxOverdue, na.rm = T),
      Bureau_AmtCreditMaxOverdue_median = median(AmtCreditMaxOverdue/N, na.rm = T),
      
      Bureau_DaysCreditEnddate_DaysEnddateFact_diff = sum(DaysCreditEnddate - DaysEnddateFact, na.rm = T),
      Bureau_DaysCreditEnddate_DaysEnddateFact_median = median((DaysCreditEnddate - DaysEnddateFact)/N, na.rm = T),
      
      Bureau_CntCreditProlong_max = max(CntCreditProlong, na.rm = T),
      Bureau_CntCreditProlong_min = max(CntCreditProlong, na.rm = T),
      Bureau_CntCreditProlong_range = Bureau_CntCreditProlong_max - Bureau_CntCreditProlong_min,
      
      Bureau_AmtCreditSum_sum = sum(AmtCreditSum, na.rm = T),
      Bureau_AmtCreditSum_median = median(AmtCreditSum/N, na.rm = T),
      
      Bureau_AmtCreditSumDebt_sum = sum(AmtCreditSumDebt, na.rm = T),
      Bureau_AmtCreditSumDebt_median = median(AmtCreditSumDebt/N, na.rm = T),
      
      Bureau_AmtCreditSumLimit_sum = sum(AmtCreditSumLimit, na.rm = T),
      Bureau_AmtCreditSumLimit_median = median(AmtCreditSumLimit/N, na.rm = T),
      
      Bureau_AmtCreditSumOverdue_sum = sum(AmtCreditSumOverdue, na.rm = T),
      Bureau_AmtCreditSumOverdue_median = median(AmtCreditSumOverdue/N, na.rm = T)
    )
}



