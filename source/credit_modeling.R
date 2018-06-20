

#'
#'
#'


#' Get model metadata
#'
#' @param dt 
#'
model.getMetadata <- function(dt) {
  stopifnot(
    is.data.frame(dt)
  )
  
  # labels info
  label <- "Label"
  
  # features info
  # set all Logic, FactorSLE, FactorOHE, ExtraVars vars explicitly
  f <- list(
    Numeric = c(),
    Logic = c("EmergencystateMode", 
              "RegRegionNotLiveRegion", "RegRegionNotWorkRegion", "LiveRegionNotWorkRegion", "RegCityNotLiveCity", "RegCityNotWorkCity", "LiveCityNotWorkCity",
              "FlagOwnCar", "FlagOwnRealty", "FlagEmpPhone", "FlagWorkPhone", "FlagContMobile", "FlagPhone", "FlagEmail",
              "FlagDocument3", "FlagDocument4", "FlagDocument5", "FlagDocument6", "FlagDocument7", "FlagDocument8", "FlagDocument9", "FlagDocument11", "FlagDocument18"),
    FactorSLE = c(), 
    FactorOHE = c("CodeGender", "NameContractType", "NameTypeSuite", "NameIncomeType", "NameEducationType", "NameFamilyStatus", "NameHousingType", "OccupationType", "OrganizationType", "FondkapremontMode", "HousetypeMode", "WallsmaterialMode"),
    ExtraVars = c("SkIdCurr"),
    Redundant = c("FlagMobil", paste0("FlagDocument", c(2, 10, 12:17, 19:21)))
  )
  
  f$Numeric <- c(
    f$Numeric,
    setdiff(names(dt), c(f$Numeric, f$Logic, f$FactorSLE, f$FactorOHE, f$ExtraVars, f$Redundant, label))
  )
  
  f$Redundant <- c(
    f$Redundant,
    setdiff(names(dt), c(f$Numeric, f$Logic, f$FactorSLE, f$FactorOHE, f$ExtraVars, f$Redundant, label))
  )
  
  write(
    sprintf("Ignored fields: %s", paste(f$Redundant, collapse = ", ")), stdout()
  )
  
  stopifnot(
    !anyDuplicated(c(f$Numeric, f$Logic, f$FactorSLE, f$FactorOHE, f$ExtraVars, f$Redundant, label)),
    ncol(dt) == length(c(f$Numeric, f$Logic, f$FactorSLE, f$FactorOHE, f$ExtraVars, f$Redundant, label))
  )
  
  
  # return result
  list(
    Features = f,
    Labels = label
  )
}
