

#'
#'
#'


#'
#'
#'
#' @param .config 
installments_payment.load <- function(.config) {
  stopifnot(
    is.list(.config)
  )
  
  fread.csv.zipped("installments_payments.csv", .config$DataDir)
}




#' 
#'
#' @param dt 
#'
installments_payment.getMetadata <- function(dt) {
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
installments_payment.clean <- function(dt, .metadata) {
  dt
}



#' 
#'
#' @param dt 
#' @param .metadata 
#'
installments_payment.convert <- function(dt, .metadata) {
  dt %>% 
    mutate_at(
      vars(starts_with("Num"), starts_with("Days")),
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
installments_payment.toWideTable <- function(dt, .groupByFields = "SkIdPrev", .prefix = "installments_payments", .fillNA = NA_real_) {
  require(dplyr)
  require(data.table)
  stopifnot(
    is.data.frame(dt),
    is.character(.groupByFields),
    is.character(.prefix),
    is.double(.fillNA)
  )
  
  names(dt) <- paste(.prefix , names(dt), sep = "__")
  
  
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


