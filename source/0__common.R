

#'
#' Common functions for jobs
#'


# Set environment ----
options(max.print = 1e3, scipen = 999, width = 1e2)
options(stringsAsFactors = F)


#' Startup job
#'
#' @param jobName Job name
#' @param mode release (default) or debug
#' @param packages List of packages
#'
#' @return Job
job.startup <- function(jobName, 
                        mode = "release", 
                        packages = c("dplyr", "tidyr", "purrr", "doParallel", "foreach")) {
  stopifnot(
    is.character(jobName) | is.null(jobName),
    is.character(mode),
    is.character(packages) | is.null(packages)
  )
  
  
  Sys.setenv(R_CONFIG_ACTIVE = mode)
  
  # core scripts ----
  source("_core.R")
  source("_dependencies.R")
  
  # job dependencies ----
  library(config, verbose = F)
  for (p in packages) library(p, character.only = T, verbose = F)
  
  # settings & config
  convertToPOSIX <- function(x) as.POSIXct(paste(x, "00:00:00"), format = "%F %T", tz = "UTC")
  
  job <- list(
    Name = jobName,
    Config = config::get(),
    Settings = list()
  )
  
  
  job
}


