

#'
#' Loan applications
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
    FactorSLE = c("NameContractType", "NameTypeSuite", "NameIncomeType", "NameEducationType", "NameFamilyStatus", "NameHousingType", "OccupationType", "OrganizationType", "FondkapremontMode", "HousetypeMode", "WallsmaterialMode"), 
    FactorOHE = c(),
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




#' 
#'
#' @param .config 
#'
loan.load <- function(.config) {
  require(dplyr)
  require(purrr)
  stopifnot(is.list(.config))
  
  
  dt <- list(
      Train = fread.csv.zipped("application_train.csv", .config$DataDir),
      Test = fread.csv.zipped("application_test.csv", .config$DataDir) %>% mutate(Target = NA_integer_)
    ) %>%
    map(
      ~ .x %>% rename(Label = Target)
    )
  
  
  stopifnot(
    length(dt) == 2,
    nrow(dt$Test) > 0,
    nrow(dt$Train) > nrow(dt$Test),
    is_empty(setdiff(names(dt$Train), names(dt$Test))),
    !anyNA(dt$Train$Label)
  )
  
  dt
}



#' Clear logic errors in loans application dataset
#'
#' @param dt 
#' @param .metadata 
#' 
loan.clear <- function(dt, .metadata) {
  require(dplyr)
  
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  
  
  dt <- dt %>% 
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
  
  
  stopifnot(
    nrow(dt) > 0,
    nrow(dt %>% select(-SkIdCurr) %>% filter_if(is.numeric, any_vars(. == 365243))) == 0,
    nrow(dt %>% filter_if(is.character, any_vars(tolower(.) %in% c("xna", "xap")))) == 0
  )
  
  dt
}



#' Format data in loans application dataset
#'
#' @param dt 
#' @param .metadata 
#' 
loan.format <- function(dt, .metadata) {
  require(dplyr)
  require(stringr)
  
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  
  
  wdays <- c(5, 2, 6, 1, 4, 3, 7); 
  names(wdays) <- c("friday", "tuesday", "saturday", "monday", "thursday", "wednesday", "sunday")
  
  dt %>% 
    # convert logic to binary format
    mutate(
      WeekdayApprProcessStart = wdays[tolower(WeekdayApprProcessStart)],
      CodeGender = if_else(tolower(CodeGender) == "m", 1L, 0L)
    ) %>% 
    mutate_at(
      c("EmergencystateMode", "FlagOwnCar", "FlagOwnRealty"),
      funs(if_else(!is.na(.),
                   if_else(. %in% c("Y", "Yes"), 1L, 0L),
                   NA_integer_)
      )
    ) %>%  
    # format factor features
    mutate_if(
      is.character,
      funs(if_else(!is.na(.), str_replace_all(str_to_lower(.), "\\W", "_"), NA_character_))
    )
}



#' 
#'
#' @param dt 
#' @param .fieldPattern 
#'
loan.calcStatsBy <- function(dt, .fieldPattern) {
  require(dplyr)
  require(tidyr)
  
  stopifnot(
    is.data.frame(dt),
    is.character(.fieldPattern)
  )
  
  
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
loan.calcRequestsNumber <- function(dt, .fillNA = NA_real_) {
  stopifnot(is.numeric(.fillNA))
  
  dt %>% 
    loan.calcStatsBy(., "AmtReq") %>% 
    mutate(AmtReq_count = replace_na(AmtReq_count, .fillNA))
}


  
#' 
#'
#' @param dt 
#'
loan.calcDocumentNumber <- function(dt) {
  dt %>% 
    loan.calcStatsBy(., "FlagDocument")
}



#' 
#'
#' @param dt 
#'
loan.calcDefaultSocialCircleNumber <- function(dt, .fillNA = NA_real_) {
  stopifnot(is.numeric(.fillNA))
  
  dt %>% 
    loan.calcStatsBy(., "CntSocialCircle") %>% 
    mutate(CntSocialCircle_count = replace_na(CntSocialCircle_count, .fillNA))
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
      OwnershipInteract = FlagOwnCar + 2 * FlagOwnRealty,
      GenderOwnershipInteract = CodeGender + FlagOwnCar + FlagOwnRealty
    )
}




#' 
#'
#' @param dt 
#'
loan.processingIncome <- function(dt) {
  require(dplyr)
  stopifnot(is.data.frame(dt))
  
  
  dt %>% 
    mutate(
      CntAdult = CntFamMembers - CntChildren,
      
      IncomePerMember = AmtIncomeTotal/CntFamMembers,
      IncomePerAdult = AmtIncomeTotal/CntAdult,
      IncomePerMemberChanging = IncomePerMember - (AmtIncomeTotal - AmtAnnuity)/CntFamMembers,
      
      IncomeToCredit_ratio = (12 * AmtIncomeTotal - AmtCredit)/(12 * AmtIncomeTotal),
      IncomeToAnnuity_ratio = (AmtIncomeTotal - AmtAnnuity)/AmtIncomeTotal,
      
      CreditDuration = AmtCredit/AmtAnnuity,
      CreditPercent = (AmtCredit - AmtGoodsPrice) / AmtCredit / CreditDuration * 12
    ) # %>% 
    # mutate_at(
    #   vars(starts_with("IncomePer")), funs(log(. + 1))
    # )
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
      ExtSource_mean = if_else(ExtSource_weight == 7,
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
    mutate_at(
      c(paste0("ExtSource", c(1:3))), 
      funs("ExtSourceMean_diff" = . - ExtSource_mean)
    ) %>% 
    select(
      -ExtSource1_w, -ExtSource2_w, -ExtSource3_w
    )
  
  # TODO: replace NAs to mean
}




#' 
#'
#' @param dt 
#' @param .train 
#'
loan.processingDays <- function(dt) {
  stopifnot(is.data.frame(dt))
  require(dplyr)
  
  
  features <- names(dt %>% select(starts_with("Days")))
  
  dt %>% 
    mutate_at(
      features,
      funs(if_else(!(is.na(.) | . > 0), as.integer(abs(.)), 0L))
    ) %>% 
    mutate_at(
      features,
      funs("years" = . %/% 365.25)
    ) %>% 
    mutate_at(
      features,
      funs("DaysBirth_ratio" = ./DaysBirth)
    )  %>% 
    mutate_at(
      features,
      funs("DaysEmployed_ratio" = ./(DaysEmployed + 1))
    ) %>% 
    mutate(
      OwnCarAge = replace_na(OwnCarAge, 0),
      OwnCarAge_DaysBirth_ratio = OwnCarAge/DaysBirth_years,
      OwnCarAge_DaysEmployed_ratio = OwnCarAge/(DaysEmployed_years + 1)
    ) %>% 
    # # normalization
    # mutate_at(
    #   vars(one_of(c("DaysEmployed_years", "DaysRegistration_years", "DaysIdPublish_years", "DaysLastPhoneChange_years"))),
    #   funs(log(. + 1))
    # )  %>% 
    # remove redundant
    select(
      #-DaysEmployed, -DaysRegistration, -DaysIdPublish, -DaysLastPhoneChange, -DaysBirth,
      -DaysBirth_DaysBirth_ratio, -DaysEmployed_DaysEmployed_ratio, -DaysBirth_DaysEmployed_ratio
    )
  
    # TODO: replace NAs
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



#' Processing missing values
#'
#' @param dt 
#' @param .metadata 
#' 
loan.missingValuesProcessing <- function(dt, .metadata) {
  require(dplyr)
  stopifnot(
    is.data.frame(dt),
    is.list(.metadata)
  )
  
  
  dt %>%
    mutate(
      EmergencystateMode = replace_na(EmergencystateMode, -1L),
      HousetypeMode = replace_na(HousetypeMode,  "block_of_flats"),
      FondkapremontMode = replace_na(FondkapremontMode, "reg_oper_account")
    ) %>% 
    mutate_if(
      is.character,
      funs(replace_na(., as.character(-1)))
    )
}
