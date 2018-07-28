

#'
#' Previous loans info from credit bureaus
#'



#'
#'
#' @param .config 
#' 
bureau.load <- function(.config) {
  stopifnot(is.list(.config))
  burea <- fread.csv.zipped("bureau.csv", .config$DataDir)
}



#' 
#'
#' @param dt 
#' @param .minObservationNumber 
#'
burea.filter <- function(dt, .minObservationNumber = 100L) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.numeric(.minObservationNumber)
  )
  
  dt %>% 
    ## prepare to filter
    mutate_if(
      is.character,
      funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
    ) %>% 
    mutate(
      CreditCurrency = if_else(CreditCurrency == "currency_1", 0L, 1L)
    ) %>% 
    ## filter by CreditActive type
    filter(
      CreditActive %in% c("closed", "active")
    ) %>% 
    ## filter rare cases
    group_by(
      CreditType, CreditActive, CreditCurrency
    ) %>% 
    filter(
      n() >= .minObservationNumber
    ) %>% 
    ungroup()
}



#' 
#'
#' @param dt 
#'
burea.preprocessing <- function(dt) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt)
  )

  dt %>% 
    mutate_at(
      c("DaysCreditEnddate", "DaysEnddateFact", "DaysCreditUpdate"),
      funs(DaysCredit_diff = . - DaysCredit)
    ) %>%
    mutate(
      CreditDaysUnderdue = DaysCreditEnddate - DaysEnddateFact
    )
}



#' 
#' 
#' @param .fillNA 
#' @param balances 
#' @param dt 
#' 
bureau.getHistoryStats <- function(dt, balances, .fillNA = NA_real_) {
  require(dplyr)

  stopifnot(
    is.data.frame(dt),
    is.data.frame(balances),
    is.numeric(.fillNA)
  )
  
  
  keyField <- "SkIdCurr"
  
  ## 1
  dt <- dt %>% 
    # join w/ stats
    left_join(balances, by = "SkIdBureau") %>% 
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
    # fill NAs if nessesary
    mutate_if(
      is.double,
      funs(replace_na(., .fillNA))
    )
}



#' Get Bureau Balances stats
#'
#' @param .config 
#' @param .fillNA 
#' @param .minSD 
#' @param .minNA 
#' 
bureau.getBalancesStats <- function(.config, 
                                    .fillNA = NA_real_,
                                    .minSD = .01, .minNA = .05) {
  require(dplyr)
  stopifnot(
    is.list(.config)
  )
  
  dt <- fread.csv.zipped("bureau_balance.csv", .config$DataDir) %>% 
    common.fe.calcStatsByGroups(.,
                                "SkIdBureau", list(".", "Status"), "MonthsBalance",
                                .fillNA, .drop = T) %>%
    select(
      -matches("_(first|last)")
    ) %>% 
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    )
  
  names(dt)[-1] <- paste0("bb__", names(dt)[-1])
  
  
  dt
}



#'
#'
#' @param dt 
#'
bureau.featureSelection <- function(dt, .minSD = .01, .minNA = .05) {
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
      -one_of(common.fe.findCorrelatedCols(., .threshold = .98, .extraFields = keyField))
    )
}


