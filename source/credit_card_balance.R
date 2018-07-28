

#'
#' Monthly data about previous credit cards clients have had with bank
#'



#'
#'
#' @param .config 
#' 
credit_card_balance.load <- function(.config) {
  stopifnot(
    is.list(.config)
  )
  
  fread.csv.zipped("credit_card_balance.csv", .config$DataDir)
}



#' 
#'
#' @param dt 
#'
credit_card_balance.getMetadata <- function(dt) {
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
#' 
credit_card_balance.clean <- function(dt) {
  dt
}



#' 
#'
#' @param dt 
#' @param .metadata 
#'
credit_card_balance.convert <- function(dt, .metadata) {
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
      vars(starts_with("Cnt"), "MonthsBalance"),
      as.integer
    ) %>% 
    mutate_at(
      vars(starts_with("Amt")), 
      as.double
    )
}



#' 
#'
#' @param dt 
#'
credit_card_balance.preprocessing <- function(dt) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt)
  )
  
  
  dt %>% 
    ## amount and count drawings
    mutate_at(
      vars(starts_with("AmtDrawings")),
      funs("AmtDrawingsCurrent_ratio" = replace_na(., 0)/(AmtDrawingsCurrent + 1))
    ) %>% 
    mutate_at(
      vars(starts_with("CntDrawings")),
      funs("CntDrawingsCurrent_ratio" = if_else(CntDrawingsCurrent != 0, ./CntDrawingsCurrent, 0))
    ) %>% 
    select(-c(
      AmtDrawingsAtmCurrent, AmtDrawingsOtherCurrent, AmtDrawingsPosCurrent, AmtDrawingsCurrent_AmtDrawingsCurrent_ratio,
      CntDrawingsAtmCurrent, CntDrawingsOtherCurrent, CntDrawingsPosCurrent, CntDrawingsCurrent_CntDrawingsCurrent_ratio
    )) %>% 
    ## balances and limits
    mutate(
      AmtBalance_AmtCreditLimitActual_ratio = (AmtBalance - AmtCreditLimitActual)/AmtCreditLimitActual,
      AmtInstMinRegularity_AmtCreditLimitActual_ratio = AmtInstMinRegularity/AmtCreditLimitActual,
      AmtPaymentCurrent_AmtRecivable_ratio = AmtPaymentCurrent/(AmtRecivable + 1)
    ) %>%
    select(-c(
      AmtCreditLimitActual, AmtInstMinRegularity, # already used in AmtBalance|AmtInstMinRegularity_AmtCreditLimitActual_ratio
      AmtPaymentTotalCurrent, AmtTotalReceivable, AmtReceivablePrincipal, # high correlated w/ AmtPaymentCurrent and AmtRecivable
      SkDpdDef  # high correlated w/ SkDpd
    ))
}



#' 
#'
#' @param dt 
#' @param .fillNA 
#' @param .minSD 
#' @param .minNA 
#'
credit_card_balance.getHistoryStats <- function(dt, 
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


