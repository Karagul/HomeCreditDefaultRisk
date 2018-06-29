

#'
#' Previous loans info from credit bureaus
#'


#'
#'
#' @param .config 
#' 
bureau.load <- function(.config, .replaceNA = 0) {
  require(dplyr)
  require(stringr)
  stopifnot(
    is.list(.config),
    is.numeric(.replaceNA)
  )

  # calculte balances stats 
  calcBalancesStats <- function() {
    require(tidyr)
    
    bureauBalances.grouped <- fread.csv.zipped("bureau_balance.csv", .config$DataDir) %>% 
      group_by(SkIdBureau, Status) %>% 
      summarise(
        N = log(n()),
        Min = min(MonthsBalance),
        Median = median(MonthsBalance),
        Max = max(MonthsBalance)
      )
    
    bureauBalances.grouped %>% select(-Min, -Max, -Median) %>% spread(Status, N, sep = "_N_", fill = .replaceNA) %>% 
      inner_join(
        bureauBalances.grouped %>% select(-Min, -N, -Median) %>% spread(Status, Max, sep = "_max_", fill = .replaceNA),
        by = "SkIdBureau") %>% 
      inner_join(
        bureauBalances.grouped %>% select(-Max, -N, -Median) %>% spread(Status, Min, sep = "_min_", fill = .replaceNA),
        by = "SkIdBureau") %>% 
      inner_join(
        bureauBalances.grouped %>% select(-Min, -Max, -N) %>% spread(Status, Median, sep = "_median_", fill = .replaceNA),
        by = "SkIdBureau")
  }
  
  
  fread.csv.zipped("bureau.csv", .config$DataDir) %>% 
    mutate_if(
      is.character,
      funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
    ) %>% 
    mutate(
      CreditCurrency = if_else(CreditCurrency == "currency_1", 0L, 1L),
      DaysCredit = abs(DaysCredit)
    ) %>% 
    filter(
      CreditActive %in% c("closed", "active")
    ) # %>% 
    # left_join(
    #   calcBalancesStats(), by = "SkIdBureau"
    # )
}



#' 
#'
#' @param dt 
#'
bureau.getHistory <- function(dt) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt)
  )
  
  
  groupByFields <- c("SkIdCurr", "CreditType", "CreditActive")
  
  dt.g <- dt %>% 
    transmute(
      # group by fields
      SkIdCurr, CreditType, CreditActive,
      # calc stats for fields
      CreditCurrency,
      DaysCredit,
      CreditDayUnderdue = DaysCreditEnddate - DaysEnddateFact,
      DaysCreditEnddate = abs(DaysCreditEnddate),
      CreditDayOverdue,
      CntCreditProlong,
      AmtCreditSum,
      AmtCreditSumDebt,
      AmtCreditSumLimit,
      AmtCreditSumOverdue,
      DaysCreditUpdate,
      AmtAnnuity
    ) %>% 
    group_by_at(
      groupByFields
    )
  
  
  dt.g %>% 
    summarise(
      N = n(),
      CreditCurrencyRisk = sum(CreditCurrency, na.rm = T)
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
    inner_join(
      dt.g %>% summarise_if(is.numeric, funs("sum" = sum(., na.rm = T))),
      by = groupByFields
    ) %>% 
    inner_join(
      dt.g %>% summarise_if(is.numeric, funs("mad" = mad(., na.rm = T))),
      by = groupByFields
    ) %>% 
    mutate_if(
      is.numeric, 
      funs(if_else(is.nan(.) | is.infinite(.), NA_real_, as.numeric(.)))
    ) %>% 
    ungroup() %>% 
    select(
      -starts_with("CreditCurrency_")
    )
}




#' 
#'
#' @param dt 
#' @param .minObservationNumber 
#' @param .minSD 
#' @param .minNotNA 
#'
bureau.getHistoryStats <- function(dt, .minObservationNumber = 100, .minSD = .01, .minNotNA = .01, .replaceNA = NA_real_) {
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
  
  
  groupByFields <- c("SkIdCurr", "CreditType", "CreditActive")
  
  dt <- dt %>% 
    group_by(CreditType, CreditActive) %>% 
    filter(n() >= .minObservationNumber) %>% 
    ungroup() %>% 
    as.data.table(., key = groupByFields)
  
  dt <- melt(dt, id.var = groupByFields, variable.name = "Metrics")
  dt <- dt[, Metrics := paste("bureau", CreditType, CreditActive, Metrics, sep = "__")]
  
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


