

#'
#'
#'


#'
#'
#'
#' @param .config 
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
#' @param .metadata 
#' 
credit_card_balance.clean <- function(dt, .metadata) {
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
#' @param .groupByFields 
#' @param .prefix 
#' @param .fillNA 
#'
credit_card_balance.toWideTable <- function(dt, .groupByFields = c("SkIdPrev", "NameContractStatus"), .prefix = "credit_card_balance", .fillNA = NA_real_) {
  require(dplyr)
  require(data.table)
  stopifnot(
    is.data.frame(dt),
    is.character(.groupByFields),
    is.character(.prefix),
    is.double(.fillNA)
  )
  
  # convert to data table
  dt <- as.data.table(dt %>% mutate_if(is.integer, as.double))
  
  dt <- melt(dt, id.var = .groupByFields, variable.name = "V")
  dt <- dt[, V := paste(.prefix, NameContractStatus, V, sep = "__")]
  
  dt <- dcast.data.table(dt, SkIdPrev ~ V, value.var = "value", fill = NA_real_)
  
  
  # remove redundant cols
  cols <- common.modeling.getRedundantCols(dt)
  if (!is_empty(cols)) {
    dt <- dt %>% select(-one_of(cols))
  } else {
    dt
  }
  
  
  # remove NAs
  dt %>% 
    select_if(
      is.numeric,
      funs(replace_na(., .fillNA))
    )
}




