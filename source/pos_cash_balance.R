

#'
#' Monthly data about previous point of sale or cash loans clients have had with bank
#'



#'
#'
#'
#' @param .config 
pos_cash_balance.load <- function(.config) {
  stopifnot(
    is.list(.config)
  )
  
  fread.csv.zipped("POS_CASH_balance.csv", .config$DataDir)
}



#' 
#'
#' @param dt 
#'
pos_cash_balance.getMetadata <- function(dt) {
  stopifnot(
    is.data.frame(dt)
  )
  
  
  key <- NULL
  label <- NULL
  extraVars <- c("SkIdCurr", "SkIdPrev")
  ignored <- NULL
  
  fetures <- list(
    Numeric = setdiff(names(dt %>% select_if(is.numeric)), c(key, label, extraVars, ignored)),
    Factors = setdiff(names(dt %>% select_if(is.character)), c(key, label, extraVars, ignored)),
    Binary = c()
  )
  

  meta <- list(
    Key = key,
    Features = fetures,
    Label = label,
    Ignored = ignored,
    ExtraVars = extraVars
  )
  
  stopifnot(
    all(unlist(meta, recursive = T, use.names = F) %in% names(dt)),
    !anyDuplicated(unlist(meta, recursive = T, use.names = F))
  )

  write(
    sprintf("Ignored fields: %s", paste(meta$Ignored, collapse = ", ")), stdout()
  )
  
  
  meta
}



#' Clear logic errors
#'
#' @param dt 
#' @param .metadata 
#' 
pos_cash_balance.clean <- function(dt, .metadata) {
  require(dplyr)
  
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  
  
  numericInvalidValues <- c(365243)
  characterInvalidValues <- c("xna", "xap")
  
  
  result <- dt %>% 
    mutate_at(
      .metadata$Features$Numeric,
      funs(ifelse(. %in% numericInvalidValues, NA, .))
    ) %>% 
    mutate_at(
      .metadata$Features$Factors,
      funs(if_else(tolower(.) %in% characterInvalidValues, NA_character_, .))
    )
  
  
  stopifnot(
    nrow(result) > 0,
    nrow(result %>% 
           select(.metadata$Features$Numeric) %>% 
           filter_all(any_vars(. %in% numericInvalidValues))
         ) == 0,
    nrow(result %>% 
           select(.metadata$Features$Factors) %>% 
           filter_all(any_vars(tolower(.) %in% characterInvalidValues))
         ) == 0
  )
  
  result
}



#' 
#'
#' @param dt 
#' @param .metadata 
#'
pos_cash_balance.convert <- function(dt, .metadata) {
  require(dplyr)
  require(stringr)
  
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  
  
  dt %>%  
    mutate_at(
      .metadata$Features$Factors,
      funs(str_replace_all(str_to_lower(.), "\\W", "_"))
    ) %>% 
    mutate_at(
      vars(starts_with("Cnt")),
      as.integer
    )
}



#' 
#'
#' @param dt 
#' @param .fillNA 
#' @param .minSD 
#' @param .minNA 
#'
pos_cash_balance.getHistoryStats <- function(dt, 
                                             .fillNA = NA_real_,
                                             .minSD = .01, .minNA = .05) {
  
  require(dplyr)
  require(tidyr)
  require(purrr)
  
  stopifnot(
    is.data.frame(dt),
    is.numeric(.fillNA),
    is.numeric(.minSD),
    is.numeric(.minNA)
  )
  
  keyField <- "SkIdPrev"
  
  values <- setdiff(names(dt %>% select_if(is.numeric)),
                    c(keyField, "SkIdCurr"))
  
  dt <- dt %>% 
    arrange(MonthsBalance) %>% 
    common.fe.calcStatsByGroups(.,
                                keyField, list(".", "NameContractStatus"), values,
                                .fillNA = NA_real_, .drop = T) %>%
    select(
      -matches("_(first|last)")
    ) %>%
    select(
      -one_of(common.fe.findRedundantCols(., .minSD, .minNA))
    )
}


