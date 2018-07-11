

#' Install packages if its not installed yet
#'
#' @param x A character vector of installing packages
#' @param repos A repositories to use
#' @return The result of installation
packages.install <- function(x, repos = getOption("repos")) {
  stopifnot(
    is.character(x),
    is.character(repos) && 
      ("https://mran.microsoft.com/snapshot/2018-04-01" %in% repos | "https://cran.r-project.org/" %in% repos)
  )
  
  packages.missing <- x[!(x %in% installed.packages()[, 1])]
  if (length(packages.missing) != 0)
  {
    install.packages(packages.missing, repos = repos)
    print(paste("Successfully installed packages:", paste(packages.missing, collapse = ", ")))
  } else {
    print("All packages have already installed")
  }
}



#' Read CSV files in the specified directory
#' 
#' @param dir 
#' @param sep 
#' @param header 
#' @param na.strings 
#' @param stringsAsFactors 
#' @param maxFilesNumber 
#' @param pattern
#' @param recursiveSearch 
#'
#' @return data table
dread.csv <- function(dir, pattern = "*.csv", recursiveSearch = T, sep = ";", header = T, na.strings = "", stringsAsFactors = F, maxFilesNumber = 1e6) {
  require(data.table)
  require(dplyr)
  require(purrr)
  
  stopifnot(
    is.character(dir),
    is.character(pattern),
    is.logical(recursiveSearch),
    is.character(sep),
    is.logical(header),
    is.character(na.strings),
    is.logical(stringsAsFactors),
    is.numeric(maxFilesNumber) && maxFilesNumber > 0
  )
  
  
  files <- list.files(dir, pattern = pattern, full.names = T, recursive = recursiveSearch)
  stopifnot(length(files) > 0)
  
  map(head(files, maxFilesNumber),
      ~ tryCatch({
        fread(.x, sep = sep, header = header, na.strings = na.strings, stringsAsFactors = stringsAsFactors)
      }, 
      error = function(ex) { 
        write(sprintf("%s: %s", .x, ex), stderr()) 
      })
  ) %>% 
    bind_rows
}



#' Read RDS files in the specified directory
#'
#' @param dir 
#' @param pattern 
#' @param recursiveSearch 
#' @param maxFilesNumber 
#'
#' @return data table
dread.RDS <- function(dir, pattern = "*.rds", recursiveSearch = T, maxFilesNumber = 1e6, selectedFields = NULL) {
  require(dplyr)
  require(purrr)
  require(bit64)
  stopifnot(
    is.character(dir),
    is.character(pattern),
    is.logical(recursiveSearch),
    is.numeric(maxFilesNumber) && maxFilesNumber > 0,
    is.character(selectedFields) | is.null(selectedFields)
  )
  
  
  files <- list.files(dir, pattern = pattern, full.names = T, recursive = recursiveSearch)
  stopifnot(length(files) > 0)
  
  map(head(files, maxFilesNumber),
      function(.x) {
        write(sprintf("Starting read %s", .x), stdout())
        
        if (is.null(selectedFields)) {
          readRDS(.x)
        } else {
          readRDS(.x) %>% 
            select(one_of(selectedFields))
        }
      }) %>% 
    bind_rows
}



#' Read RDSs from cache
#'
#' @param pattern 
#' @param cacheDir 
#' @param dataVersion 
#' @param recursiveSearch 
#' @param maxFilesNumber 
#' @param selectedFields 
#' 
dreadFromCache.RDS <- function(cacheDir, dataVersion, pattern = "*.rds", recursiveSearch = T, maxFilesNumber = 1e6, selectedFields = NULL) {
  stopifnot(
    is.character(cacheDir),
    is.character(dataVersion)
  )
  
  dread.RDS(sprintf("%s/v%s", cacheDir, dataVersion), pattern, recursiveSearch, maxFilesNumber, selectedFields)
}



#' Read zipped CSV file
#'
#' @param fileName 
#' @param dir 
#' @param .header 
#' @param .sep 
#' @param .na_strings 
#' @param .stringsAsFactors 
#'
fread.csv.zipped <- function(fileName, dir, .header = T, .sep = ",", .na_strings = "", .stringsAsFactors = F) {
  require(data.table)
  require(dplyr)
  require(purrr)
  require(stringr)
  
  stopifnot(
    is.character(fileName),
    is.character(dir),
    is.character(.sep),
    is.character(.na_strings),
    is.logical(.header),
    is.logical(.stringsAsFactors)
  )
  
  read.table(
      unz(sprintf("%s/%s.zip", dir, fileName), fileName), 
      header = .header, sep = .sep, na.strings = .na_strings, stringsAsFactors = .stringsAsFactors) %>% 
    set_names(
      map(
        names(.),
        ~ paste(str_to_title(str_split(.x, "_")[[1]]), collapse = "")
      ) %>%
      flatten_chr 
    )
}

