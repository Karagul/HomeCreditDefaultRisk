

#'
#' Previous loans info from credit bureaus
#'


# Import dependencies
source("common_fe.R")


#'
#'
#' @param .config 
#' 
bureau._load <- function(.config, .fillNA) {
  burea <- fread.csv.zipped("bureau.csv", .config$DataDir) %>% 
    mutate_if(
      is.character,
      funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
    ) %>% 
    mutate(
      CreditCurrency = if_else(CreditCurrency == "currency_1", 0L, 1L)
    ) %>% 
    filter(
      CreditActive %in% c("closed", "active")
    )
}



#' Get Bureau Balances stats
#'
bureau._getBalancesStats <- function(.config, .fillNA, .minSD, .minNA) {
  fread.csv.zipped("bureau_balance.csv", .config$DataDir) %>% 
    common.fe.calcStatsByGroups(.,
                                "SkIdBureau", list(".", "Status"), "MonthsBalance",
                                .fillNA, .drop = T) %>%
    select(
      -matches("_(first|last)")
    ) %>% 
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    )
}



#' 
#' @param .config 
#' @param .minObservationNumber 
#' @param .fillNA 
#' @param .minNA 
#' @param .minSD 
#' 
bureau.getHistoryStats <- function(.config, 
                                   .fillNA = NA_real_,
                                   .minObservationNumber = 100, 
                                   .minSD = .01, .minNA = .1) {
  require(dplyr)
  require(data.table)
  require(purrr)
  require(psych)
  require(stringr)
  
  stopifnot(
    is.list(.config),
    is.numeric(.fillNA),
    is.numeric(.minObservationNumber),
    is.numeric(.minSD),
    is.numeric(.minNA)
  )
  
  
  keyField <- "SkIdCurr"
  
  ## 1
  dt <- bureau._load(.config, .fillNA) %>% 
    # add features
    mutate(
      CreditDayUnderdue = DaysCreditEnddate - DaysEnddateFact
    ) %>% 
    # filter rare cases
    group_by(
      CreditType, CreditActive, CreditCurrency
    ) %>% 
    filter(
      n() >= .minObservationNumber
    ) %>% 
    ungroup() %>% 
    # join w/ stats
    left_join(
      bureau._getBalancesStats(.config, .fillNA, .minSD, .minNA),
      by = "SkIdBureau"
    ) %>% 
    # order by days before current application did client apply for Credit Bureau credit
    arrange(DaysCredit)
  
  
  ## 2
  values <- setdiff(names(dt %>% select_if(is.numeric)),
                    c(keyField, "SkIdBureau", "CreditType", "CreditActive", "CreditCurrency"))
  
  dt %>% 
    common.fe.calcStatsByGroups(.,
                                keyField, 
                                list(".", "CreditType", "CreditActive", c("CreditType", "CreditActive", "CreditCurrency")),
                                values,
                                .fillNA = NA_real_, .drop = T) %>% 
    # remove redundant fields by mask
    select(
      -matches("_min_([[:alnum:]]_)?max"),
      -matches("_max_([[:alnum:]]_)?min"),
      -matches("_length_([[:alnum:]]_)+length"),
      -matches("_(min|median|max)_([[:alnum:]]_)?length"),
      -matches("_first")
    ) %>% 
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

