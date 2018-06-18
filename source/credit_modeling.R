

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
    Logic = c("EmergencystateMode", "CodeGender", "FlagOwnCar", "FlagOwnRealty", "FlagMobil", "FlagEmpPhone", "FlagWorkPhone", "FlagContMobile", "FlagPhone", "FlagEmail", "FlagDocument2", "FlagDocument3", "FlagDocument4", "FlagDocument5", "FlagDocument6", "FlagDocument7", "FlagDocument8", "FlagDocument9", "FlagDocument10", "FlagDocument11", "FlagDocument12", "FlagDocument13", "FlagDocument14", "FlagDocument15", "FlagDocument16", "FlagDocument17", "FlagDocument18", "FlagDocument19", "FlagDocument20", "FlagDocument21"),
    FactorSLE = c(), 
    FactorOHE = c("NameContractType", "NameTypeSuite", "NameIncomeType", "NameEducationType", "NameFamilyStatus", "NameHousingType", "OccupationType", "WeekdayApprProcessStart", "OrganizationType", "FondkapremontMode", "HousetypeMode", "WallsmaterialMode"),
    ExtraVars = c("SkIdCurr"),
    Redundant = c()
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
