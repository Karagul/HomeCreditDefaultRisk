

#'
#' Feature engineering functions
#'



#' 
#'
#' @param dt 
#' @param .formula 
#' @param .values 
#' @param .fillNA 
#'
common.fe.calcStatsByGroup <- function(dt, 
                                       .formula, .values, 
                                       .fillNA = NA_real_, .drop = F) {
  require(dplyr)
  require(data.table)
  require(purrr)
  
  stopifnot(
    is.data.frame(dt),
    is_formula(.formula),
    is_character(.values) && is_vector(.values),
    is_double(.fillNA),
    is_logical(.drop)
  )
  
  print(.formula)
  
  df <- dt %>% 
    mutate_if(is.integer, as.double) %>% 
    as.data.table
  
  df %>% 
    data.table::dcast(., 
                      formula = .formula, 
                      fun = list(min, mean, median, max), # list(min, median, mean, max, sum, mad, sd)
                      value.var = .values,
                      drop = .drop,
                      fill = .fillNA,
                      na.rm = T) %>% 
    inner_join(data.table::dcast(df,
                                 formula = .formula,
                                 fun = list(first, last),
                                 value.var = .values,
                                 fill  = .fillNA,
                                 drop = .drop)) %>%
    inner_join(data.table::dcast(df, 
                                 formula = .formula,
                                 fun = list(length, is.na), # hack: is.na need to naming purposes
                                 value.var = .values[1],
                                 drop = .drop)) %>% 
    mutate_if(
      is.double,
      funs(if_else(is.nan(.) | is.infinite(.), .fillNA, .))
    ) %>% 
    mutate_if(
      is.integer,
      funs(if_else(. == 0L, as.integer(.fillNA), .))
    ) %>% 
    select(
      -contains("_is.na")
    )
}



#' 
#'
#' @param dt 
#' @param .idField 
#' @param .groupByList 
#' @param .values 
#' @param .fillNA 
#' @param .drop 
#'
common.fe.calcStatsByGroups <- function(dt, 
                                        .idField, .groupByList, .values,
                                        .fillNA = NA_real_, .drop = F) {
  require(dplyr)
  require(purrr)
  stopifnot(
    is.character(.idField)
  )
  
  
  .groupByList %>% 
    map(
      ~ as.formula(sprintf("%s ~ %s",
                           .idField,
                           paste(.x, collapse = " + ")))) %>% 
    map(
      ~ common.fe.calcStatsByGroup(dt, .x, .values, .fillNA, .drop)
    ) %>% 
    bind_cols %>% 
    select(
      -matches(sprintf("^(%s){1}\\d+$", .idField)) # remove duplicates key fields
    )
}



#' 
#'
#' @param dt 
#' @param .sd 
#' @param .NA 
#' @param .verbose 
#'
common.fe.findRedundantCols <- function(dt, .sd = .01, .NA = .01, .verbose = T) {
  require(dplyr)
  require(purrr)
  require(psych)
  
  stopifnot(
    is.data.frame(dt),
    is.numeric(.sd),
    is.numeric(.NA),
    is.logical(.verbose)
  )
  
  
  desc <- describe(dt) %>% 
    filter(sd <= .sd | n/nrow(dt) <= .NA) %>% 
    select(vars) %>% 
    as_vector
  
  n <- names(dt)[desc]
  
  if(.verbose & length(n) > 0) {
    write(sprintf("[TRACE] Detection %s redundant columns...\n%s", length(n), paste(head(n, 50L), collapse = ", ")), stdout())
  }
  
  n
}



local({
  library(psych)
  library(dplyr)
  
  
  ## prepare data
  sampleSize <- 100L
  
  dt <- data.frame(
    Id = sample(1:2, sampleSize, replace = T),
    G1 = sample(c("A1", "A2"), sampleSize, replace = T),
    G2 = sample(c("B1", "B2", "B3"), sampleSize, replace = T),
    G3 = sample(c("C1", "C2", "C3", "C4"), sampleSize, replace = T),
    V1 = sample(c(NA_real_, 1:10), sampleSize, T),
    V2 = sample(c(NA_integer_, 0L:1L), sampleSize, T),
    Order = 1:sampleSize
  ) %>% 
  arrange(Order)
  
  
  ## calcStatsInGroups tests
  formula <- Id ~ G1 + G2
  values <- c("V1", "V2")
  
  x <- common.fe.calcStatsByGroup(dt, formula, values)
  
  stopifnot(
    nrow(x) > 0,
    nrow(x) == length(unique(dt$Id)),
    ncol(x) == 1 + 2 * length(unique(dt$G1)) * length(unique(dt$G2)) * 7 - 6, # key_field + (V1 + V2) * G1 * G2 * metrics_count - length_abundance
    
    describe(x %>% select(starts_with("V1_min")))$min >= 0,
    describe(x %>% select(starts_with("V1_max")))$max <= 10,
    describe(x %>% select(starts_with("V2_min")))$min >= 0,
    describe(x %>% select(starts_with("V2_max")))$max <= 1,
    
    nrow(x %>% filter_all(any_vars(is.nan(.) | is.infinite(.)))) == 0
  )
  
  
  
  ## common.fe.findRedundantCols naive test
  x %>% select(one_of(common.fe.findRedundantCols(x)))
  
  
  
  ## calcStatsInGroupsCombination tests
  groupByList <- list(
    c("G1"),
    c("G3"),
    c("G1", "G2"),
    c("G1", "G2", "G3")
  )
  
  y <- common.fe.calcStatsByGroups(dt, "Id", groupByList, values, .drop = T)
  stopifnot(
    nrow(y) > 0,
    nrow(y) == length(unique(dt$Id)),
    ncol(y %>% select(contains("Id"))) == 1,
    
    describe(y %>% select(starts_with("V1_min")))$min >= 0,
    describe(y %>% select(starts_with("V1_max")))$max <= 10,
    describe(y %>% select(starts_with("V2_min")))$min >= 0,
    describe(y %>% select(starts_with("V2_max")))$max <= 1,
    
    nrow(y %>% filter_all(any_vars(is.nan(.) | is.infinite(.)))) == 0
  )
})



#' 
#'
#' @param dt 
#' @param .threshold 
#' @param .extraFields 
#' @param .names 
#' @param .verbose 
#'
common.fe.findCorrelatedCols <- function(dt, .threshold = .98, .extraFields = NULL, .names = T, .verbose = T) {
  require(caret)
  require(dplyr)
  
  stopifnot(
    is.data.frame(dt),
    is.double(.threshold),
    is.character(.extraFields) | is.null(.extraFields),
    is.logical(.names),
    is.logical(.verbose)
  )
  
  
  r <- dt %>% select_if(is.numeric)
  if (!is.null(.extraFields)) {
    r <- r %>% select(-one_of(.extraFields))
  }
  
  m <- cor(r, use = "pairwise.complete.obs", method = "pearson") %>% replace_na(., 0)

  
  f <- findCorrelation(m, cutoff = .threshold, names = .names)
  
  if(.verbose & length(f) > 0) {
    write(sprintf("[TRACE] Detection %s high correlated columns...\n%s", length(f), paste(head(f, 50L), collapse = ", ")), stdout())
  }
  
  f
}


local({
  library(dplyr)
  
  sampleSize <- 100L
  
  dt <- data.frame(
      Id = 1:sampleSize, # must be ignored
      C1 = sample(letters[1:2], sampleSize, replace = T),  # must be ignored
      V1 = sample(1:100, sampleSize, replace = T)
    ) %>% 
    mutate(
      V2 = -V1, # cor(V1, V2) == -1
      V3 = rnorm(sampleSize, 0, 10) + V1, # cor(V1, V2) ~ .9
      V4 = c(runif(sampleSize - 10, 1L, 100L), rep(NA_real_, 10))
    )
  
  stopifnot(
    nrow(dt) > 0,
    ncol(dt %>% select_if(is.numeric)) > 0,
    common.fe.findCorrelatedCols(dt, .threshold = .6, .extraFields = "Id") == c("V1", "V2")
  )
})


