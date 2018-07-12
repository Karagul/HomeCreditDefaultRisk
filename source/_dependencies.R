

#'
#' Install required libraries
#'


local({
  # packages list
  packages <- c("odbc", "data.table", "dplyr", "purrr", "tidyr", # data transform
                "microbenchmark", "config", "curl", "RCurl", "httr", "devtools", "reticulate", # tools
                "fasttime", "scales", "stringr", "bit64", # data processing
                "ggplot2", # vizualization
                "zoo", "xts", "Quandl", # time-series
                "foreach", "doParallel", # parallel computing
                "PRROC", "e1071", "caret" # ML algos
  )
  
  # install packages
  packages.install("lubridate", "https://cran.r-project.org/")
  packages.install(packages) 
})

