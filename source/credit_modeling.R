

#'
#'
#'


#' Get model metadata
#'
#' @param dt 
#'
loan.getMetadata <- function(dt) {
  stopifnot(
    is.data.frame(dt)
  )
  
  # labels info
  label <- "Label"
  
  # features info
  # set all Logic, FactorSLE, FactorOHE, ExtraVars vars explicitly
  f <- list(
    Numeric = c(),
    Logic = c("EmergencystateMode", "CodeGender",
              "RegRegionNotLiveRegion", "RegRegionNotWorkRegion", "LiveRegionNotWorkRegion", "RegCityNotLiveCity", "RegCityNotWorkCity", "LiveCityNotWorkCity",
              "FlagOwnCar", "FlagOwnRealty", "FlagEmpPhone", "FlagWorkPhone", "FlagContMobile", "FlagPhone", "FlagEmail",
              "FlagDocument3", "FlagDocument4", "FlagDocument5", "FlagDocument6", "FlagDocument7", "FlagDocument8", "FlagDocument9", "FlagDocument11", "FlagDocument18"),
    FactorSLE = c(), 
    FactorOHE = c("NameContractType", "NameTypeSuite", "NameIncomeType", "NameEducationType", "NameFamilyStatus", "NameHousingType", "OccupationType", "OrganizationType", "FondkapremontMode", "HousetypeMode", "WallsmaterialMode"),
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



#' Clear logic errors in loans application dataset
#'
#' @param dt 
#' @param .metadata 
#' 
loan.clear <- function(dt, .metadata) {
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  require(dplyr)
  
  dt %>% 
    filter(
      CodeGender != "XNA" & # XNA is invalid gender
      !is.na(CntFamMembers)
    ) %>% 
    mutate(
      OrganizationType = if_else(OrganizationType != "XNA", OrganizationType, NA_character_), # XNA is invalid organization type
      NameIncomeType = if_else(NameIncomeType != "Maternity leave", NameIncomeType, NA_character_), # there is no 'Maternity leave' in test dataset
      NameFamilyStatus = if_else(NameFamilyStatus != "Unknown", NameFamilyStatus, NA_character_), # replace unknown to NA
      OwnCarAge = if_else(OwnCarAge %in% c(64, 65), NA_real_, OwnCarAge),
      DaysEmployed = if_else(DaysEmployed == 365243, NA_integer_, DaysEmployed)
    ) %>%
    # remove redundant cols
    select(
      -one_of(.metadata$Features$Redundant)
    )
}



#' Format data in loans application dataset
#'
#' @param dt 
#' @param .metadata 
#' 
loan.format <- function(dt, .metadata) {
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  require(dplyr)
  require(stringr)
  
  
  wdays <- c(5, 2, 6, 1, 4, 3, 7); 
  names(wdays) <- c("friday", "tuesday", "saturday", "monday", "thursday", "wednesday", "sunday")
  
  dt %>% 
    # convert logic to binary format
    mutate_at(
      setdiff(.metadata$Features$Logic, names(dt %>% select_if(is.integer))),
      funs(if_else(!is.na(.),
                   if_else(. %in% c("Y", "Yes"), 1L, 0L),
                   NA_integer_)
      )
    ) %>%  
    # format factor features
    mutate_at(
      .metadata$Features$FactorOHE,
      funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
    ) %>% 
    mutate(
      WeekdayApprProcessStart = wdays[tolower(WeekdayApprProcessStart)],
      CodeGender = if_else(CodeGender == "m", 1L, 0L)
    )
}



#' Processing missing values
#'
#' @param dt 
#' @param .metadata 
#' 
loan.missingValuesProcessing <- function(dt, .metadata) {
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  require(dplyr)
  
  
  dt %>% 
    ## logic features processing
    mutate(
      HousetypeMode = if_else(!is.na(HousetypeMode), HousetypeMode, "block_of_flats"), # replace to mode value
      FondkapremontMode = if_else(!is.na(FondkapremontMode), FondkapremontMode, "reg_oper_account") # replace to mode value
    ) %>% 
    ## processing missing data
    # numeric
    mutate_at(
      intersect(model.metadata$Features$Numeric, names(dt)),
      funs("missing" = if_else(is.na(.), 1L, 0L))
    ) %>% 
    mutate_at(
      intersect(model.metadata$Features$Numeric, names(dt)),
      funs(ifelse(!is.na(.), ., 0))
    ) %>% 
    # logic
    mutate_at(
      intersect(model.metadata$Features$Logic, names(dt)),
      funs("missing" = if_else(is.na(.), 1L, 0L))
    ) %>%
    mutate_at(
      intersect(model.metadata$Features$Logic, names(dt)), # all must be integer
      funs(if_else(!is.na(.), ., -1L))
    ) %>%
    # factor
    mutate_at(
      intersect(model.metadata$Features$FactorOHE, names(dt)),
      funs("missing" = if_else(is.na(.), 1L, 0L))
    ) %>%
    mutate_at(
      intersect(model.metadata$Features$FactorOHE, names(dt)), # all must be character
      funs(if_else(!is.na(.), ., "none"))
    )
}




#' 
#'
#' @param dt 
#' @param .fieldPattern 
#'
loan.calcCounter <- function(dt, .fieldPattern) {
  stopifnot(
    is.data.frame(dt),
    is.character(.fieldPattern)
  )
  require(dplyr)
  require(tidyr)
  
  
  stats <- dt %>% 
    select(
      SkIdCurr, matches(.fieldPattern)
    ) %>% 
    gather(
      Key, Value, -SkIdCurr
    ) %>% 
    group_by(SkIdCurr) %>% 
    summarise(
      count = sum(Value)
    )
  
  dt %>% 
    inner_join(stats, by = "SkIdCurr") %>% 
    rename_at(
      vars(count), funs(paste(.fieldPattern, ., sep = "_"))
    )
}
  


#' 
#'
#' @param dt 
#'
loan.calcRequestsNumber <- function(dt) {
  dt %>% 
    loan.calcCounter(., "AmtReq") %>% 
    mutate(
      AmtReq_missed = if_else(!is.na(AmtReq_count), 0L, 1L),
      AmtReq_count = replace_na(AmtReq_count, 0),
      AmtReq_count = log(AmtReq_count + 1)
    )
}


  
#' 
#'
#' @param dt 
#'
loan.calcDocumentNumber <- function(dt) {
  dt %>% 
    loan.calcCounter(., "FlagDocument")
}



#' 
#'
#' @param dt 
#'
loan.calcDefaultSocialCircleNumber <- function(dt) {
  dt %>% 
    loan.calcCounter(., "CntSocialCircle") %>% 
    mutate(
      CntSocialCircle_missed = if_else(!is.na(CntSocialCircle_count), 0L, 1L),
      CntSocialCircle_count = replace_na(CntSocialCircle_count, 0),
      CntSocialCircle_count = log(CntSocialCircle_count + 1)
    )
}



#' 
#'
#' @param dt 
#'
loan.processingOwnership <- function(dt) {
  stopifnot(is.data.frame(dt))
  require(dplyr)
  
  dt %>% 
    mutate(
      OwnershipInteract = FlagOwnCar + FlagOwnRealty,
      GenderOwnershipInteract = CodeGender + FlagOwnCar + FlagOwnRealty
    )
}




#' 
#'
#' @param dt 
#'
loan.processingIncome <- function(dt) {
  stopifnot(is.data.frame(dt))
  require(dplyr)
  
  dt %>% 
    mutate(
      CntAdult = CntFamMembers - CntChildren,
      
      IncomePerMember = AmtIncomeTotal/CntFamMembers,
      IncomePerAdult = AmtIncomeTotal/CntAdult,
      
      IncomePerMemberDiff = IncomePerMember - (AmtIncomeTotal - AmtAnnuity)/CntFamMembers,
      IncomePerAdultDiff = IncomePerAdult - (AmtIncomeTotal - AmtAnnuity)/CntAdult,
      
      IncomeToCreditRate = (12 * AmtIncomeTotal - AmtCredit)/(12 * AmtIncomeTotal),
      IncomeToAnnuityRate = (AmtIncomeTotal - AmtAnnuity)/AmtIncomeTotal,
      
      CreditDuration = ceiling(AmtCredit/AmtAnnuity),
      CreditPercent = (AmtCredit - AmtGoodsPrice) / AmtCredit / CreditDuration * 12
    ) %>% 
    mutate_at(
      vars(starts_with("IncomePer")), funs(log(. + 1))
    )
}




#' 
#'
#' @param dt 
#'
loan.processingExternalSourceScore <- function(dt) {
  stopifnot(is.data.frame(dt))
  require(dplyr)
  
  dt %>% 
    mutate(
      ExtSource1_w = if_else(!is.na(ExtSource1), 1L, 0L),
      ExtSource2_w = if_else(!is.na(ExtSource2), 4L, 0L),
      ExtSource3_w = if_else(!is.na(ExtSource3), 2L, 0L),
      ExtSource_weight = ExtSource1_w + ExtSource2_w + ExtSource3_w,
      ExtSource_sum = if_else(ExtSource_weight == 7,
                              (ExtSource1 + ExtSource2 + ExtSource3)/3,
                              if_else(ExtSource_weight == 6,
                                      (ExtSource2 + ExtSource3)/2,
                                      if_else(ExtSource_weight == 5,
                                              (ExtSource1 + ExtSource2)/2,
                                              if_else(ExtSource_weight == 3,
                                                      (ExtSource1 + ExtSource3)/2,
                                                      if_else(ExtSource_weight == 4,
                                                              ExtSource2,
                                                              if_else(ExtSource_weight == 2,
                                                                      ExtSource3,
                                                                      if_else(ExtSource_weight == 1,
                                                                              ExtSource1,
                                                                              NA_real_))))))),
      ExtSource1_2_diff = ExtSource1 - ExtSource2,
      ExtSource2_3_diff = ExtSource2 - ExtSource3,
      ExtSource3_1_diff = ExtSource3 - ExtSource1
    ) %>% 
    select(-ExtSource1_w, -ExtSource2_w, -ExtSource3_w)
}




#' 
#'
#' @param dt 
#' @param .train 
#'
loan.processingDays <- function(dt) {
  stopifnot(is.data.frame(dt))
  require(dplyr)
  
  
  dt %>% 
    mutate_at(
      vars(starts_with("Days")),
      funs("y" = abs(. %/% 365.25))
    ) %>% 
    mutate_at(
      vars(ends_with("_y")),
      funs("rate" = ./DaysBirth_y)
    ) %>% 
    mutate_at(
      vars(one_of(c("DaysEmployed_y", "DaysRegistration_y", "DaysIdPublish_y", "DaysLastPhoneChange_y"))),
      funs(log(. + 1))
    ) %>% 
    select(
      -DaysBirth_y_rate, -DaysBirth, -DaysEmployed, -DaysRegistration, -DaysIdPublish, -DaysLastPhoneChange
    )
  
    # task: replace NAs
    # inner_join(.train %>%
    #              transmute(DaysEmployed, DaysBirth, ClientAge = abs(DaysBirth %/% 365.25)) %>% 
    #              group_by(ClientAge) %>% 
    #              summarise(
    #                WorkExprience_min = min(DaysEmployed/DaysBirth, na.rm = T),
    #                WorkExprience_median = median(DaysEmployed/DaysBirth, na.rm = T)
    #              ),
    #            by = "ClientAge") %>% 
    # mutate(
    #   WorkingRatePessimistic = if_else(!is.na(WorkingRate), WorkingRate, WorkExprience_min),
    #   WorkingRateOptimistic = if_else(!is.na(WorkingRate), WorkingRate, WorkExprience_median)
    # ) %>% 
    # select(
    #   -WorkingRate, -WorkExprience_min, -WorkExprience_median
    # )
}







