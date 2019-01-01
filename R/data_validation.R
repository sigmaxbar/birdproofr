#'
#'Validate all columns, then store issues as a list of data frames
#'
#'@param df bird data frame
#'@return list of issue data frames
#'
validate_all_list <- function(df) {
  all_list <- list(validate_species(df), validate_age(df), validate_sex(df),
       validate_age_bp_cp(df), validate_sex_hs(df), validate_bp_hs(df),
       validate_cp_hs(df), validate_ffmolt(df), validate_bp(df),
       validate_cp(df), validate_fat(df), validate_bmlt(df),
       validate_ffwear(df), validate_muscle(df), validate_age_skull(df),
       validate_ha_skull(df), validate_location(df), validate_bandsize(df),
       validate_bandsize_disp(df), validate_age_ffmlt(df), validate_ha_ffmlt(df),
       validate_ha_ffwear(df), validate_age_ffwear(df), validate_wing(df),
       validate_tail(df), validate_weight(df), validate_bandcode(df),
       validate_bandcode_species(df), validate_status(df), validate_status_500(df),
       validate_disp(df), validate_disp_status(df), validate_year(df),
       validate_year_species(df), validate_month(df), validate_month_species(df),
       validate_day(df), validate_day_species(df), validate_captime(df),
       validate_net(df), validate_notes(df), validate_ey(df),
       validate_age_ha(df), validate_age_hs(df), validate_parasites(df),
       validate_ha_ha2(df), validate_hs_hs2(df)
  )
  return(all_list)
}



#'
#'Validate species column. Refer to master species list to update
#'
#'@param df bird data frame
#'@return data frame of rows with species issues
#'
validate_species <- function(df) {
  valid_species_list <- c("amgo", "amke", "amre", "amro", "auwa", "bade", "balo", "bcch", "bewr", "bggn",
                          "bhco", "bhgr", "brcr", "brsp", "btyw", "buor", "bush", "cafi", "cahu", "canw", "caqu", "cavi", "cbch",
                          "cedw", "chsp", "coha", "coni", "copo", "deju", "dowo", "dufl", "evgr", "flow", "fosp", "gcki", "gcsp",
                          "grca", "grfl", "gtto", "gwcs", "hafl", "hawo", "heth", "hewa", "hofi", "howr", "lazb", "lefl", "lego",
                          "lisp", "mgwa", "moch", "mwcs", "mywa", "nawa", "nofl", "nopo", "ocwa", "orju", "osfl", "pawr", "pisi",
                          "rbnu", "rcki", "recr", "rnsa", "rowr", "rsfl", "sath", "savs", "sosp", "spto", "ssha", "stja", "swth",
                          "tewa", "toso", "towa", "udej", "uyrw", "vath", "vesp", "wavi", "wbnu", "wcsp", "webl", "wefl", "weta",
                          "wewp", "wifl", "wiwa", "ybch", "yewa", "yrwa")

  species <- tolower(df$SPECIES)
  species_issues <- filter(df, !(species %in% valid_species_list))
  if(nrow(species_issues) != 0) {
    species_issues[,"Issue"] <- "Species is rare or does not exist"
  }
  return(species_issues)
}

#'
#'Validate age column. Acceptable ages are: 0,1,2,4,5,6
#'--flag any records with blank age
#'
#'@param df bird data frame
#'@return data frame of rows with age issues
#'
validate_age <- function(df) {
  valid_age_list <- c(0,1,2,3,4,5,6)
  age_issues <- filter(df, !(AGE %in% valid_age_list))
  if(nrow(age_issues) != 0) {
    age_issues[,"Issue"] <- "Invalid age. Age must be 0 1 2 4 5 or 6"
  }
  return(age_issues)
}

#'
#'Validate sex column. Acceptable values= M F U--flag all the blanks
#'
#'@param df bird data frame
#'@return data frame of rows with sex issues
#'
validate_sex <- function(df) {
  sex_issues <- filter(df, !(SEX == "M" | SEX == "F" | SEX == "U"))
  if(nrow(sex_issues) != 0) {
    sex_issues[,"Issue"] <- "Invalid sex. Sex must be M F or U"
  }
  return(sex_issues)
}

#'
#'Check that age and BP/CP match. Age 2, 4, and 0 should always have 0 for both
#'BP and CP
#'
#'@param df bird data frame
#'@return data frame of rows with age/BP/CP issues
#'
validate_age_bp_cp <- function(df) {
  bpcp_issues <- filter(df, AGE == 0 | AGE == 2 | AGE == 4, BP != 0, CP != 0)
  if(nrow(bpcp_issues) != 0) {
    bpcp_issues[,"Issue"] <- "BP and CP must both be 0 if age = 0 2 or 4."
  }
  return(bpcp_issues)
}

#'
#'Validate how sexed and sex combinations.  Allowable values include: PL,
#'EY,FF,MB,PC,LP,NL,MR,SK,TS, (blank only in second field, or for age 0)
#'
#'F: PL,BP,WL--first HS field can NOT be blank
#'
#'M: PL,CL,WL--first HS field can NOT be blank
#'
#'U: always blank, or IC, If not blank, check hard copy for errors or
#'white-out. If sex is whited out, leave as U. Check fields above and below to
#'make sure there's not a data entry error
#'
#'@param df bird data frame
#'@return data frame of rows with hs/sex issues
#'
validate_sex_hs <- function(df) {
  sex_hs_issues <- filter(df,
                          (SEX == "F" & !(HS == "PL" | HS == "BP" | HS == "WL")) |
                            (SEX == "M" & !(HS == "PL"| HS == "CL"| HS == "WL")) |
                            (SEX == "U" & !(is.na(NA) | HS == "IC"))
  )
  if(nrow(sex_hs_issues) != 0) {
    sex_hs_issues[,"Issue"] <- "Sex-how sexed mismatch"
  }
  return(sex_hs_issues)
}

#'
#'Validate how sexed and BP for females. If sexed by BP, BP value cannot
#'be blank or 0
#'
#'@param df bird data frame
#'@return data frame of rows with BP/how sexed issues for females
#'
validate_bp_hs <- function(df) {
  fhs_bp_issues <- filter(df, SEX == "F", HS == "BP", BP == 0 | is.na(BP))
  if(nrow(fhs_bp_issues) != 0) {
    fhs_bp_issues[,"Issue"] <- "If sexed by BP BP value cannot be blank or 0"
  }
  return(fhs_bp_issues)
}

#'
#'Validate how sexed and CP for males. If sexed by CL, CP value cannot
#'be blank, 0, or 1 (i.e. CP must = 2 or 3)
#'
#'@param df bird data frame
#'@return data frame of rows with CP/how sexed issues for males
#'
validate_cp_hs <- function(df) {
  mhs_cp_issues <- filter(df, SEX == "M", HS == "CL", !(CP == 2 | CP == 3))
  if(nrow(mhs_cp_issues) != 0) {
    mhs_cp_issues[,"Issue"] <- "If sexed by CL CP value must be 2 or 3"
  }
  return(mhs_cp_issues)
}

#'
#'Validate flight feather molt. Allowable values: N, S, J, A, blank
#'
#'@param df bird data frame
#'@return data frame of rows with ffmolt issues
#'
validate_ffmolt <- function(df) {
  ffmolt_issues <- filter(df, !(is.na(FF.MLT) | FF.MLT == "N" | FF.MLT == "S" | FF.MLT == "J" | FF.MLT == "A"))
  if(nrow(ffmolt_issues) != 0) {
    ffmolt_issues[, "Issue"] <- "Invalid FF molt value. Acceptable values are N S J A and blank"
  }
  return(ffmolt_issues)
}

#'
#'Validate BP (0-5, blank okay)
#'
#'@param df bird data frame
#'@return data frame of rows with BP issues
#'
validate_bp <- function(df) {
  bp_issues <- filter(df, !is.na(BP), (BP > 5 | BP < 0))
  if(nrow(bp_issues) != 0) {
    bp_issues[, "Issue"] <- "BP cannot exceed 5"
  }
  return(bp_issues)
}

#'
#'Validate CP (0-3 allowed, blank okay)
#'
#'@param df bird data frame
#'@return data frame of rows with CP issues
#'
validate_cp <- function(df) {
  cp_issues <- filter(df, !is.na(CP), (CP > 3 | CP < 0))
  if(nrow(cp_issues) != 0) {
    cp_issues[, "Issue"] <- "CP cannot exceed 3"
  }
  return(cp_issues)
}

#'
#'Validate fat 0-5, blank are allowed. 6 fat is okay but only if
#'there's a note
#'
#'@param df bird data frame
#'@return data frame of rows with fat issues
#'
validate_fat <- function(df) {
  fat_issues <- filter(df, (FAT < 0 | FAT > 5), is.na(NOTES))
  if(nrow(fat_issues) != 0) {
    fat_issues[, "Issue"] <- "Fat cannot exceed 5"
  }
  return(fat_issues)
}

#'
#'Validate body molt. Allowable values: 0-4, blank
#'
#'@param df bird data frame
#'@return data frame of rows with body molt issues
#'
validate_bmlt <- function(df) {
  bmlt_issues <- filter(df, !is.na(B.MLT), (B.MLT > 4 | B.MLT < 0))
  if(nrow(bmlt_issues) != 0) {
    bmlt_issues[, "Issue"] <- "Body molt cannot exceed 4"
  }
  return(bmlt_issues)
}

#'
#'Validate flight feather wear. Allowable values: 0-5, blank
#'
#'@param df bird data frame
#'@return data frame of rows with ffwear issues
#'
validate_ffwear <- function(df) {
  ffwear_issues <- filter(df, !is.na(FF.WEAR), (FF.WEAR > 5 | FF.WEAR < 0))
  if(nrow(ffwear_issues) != 0) {
    ffwear_issues[, "Issue"] <- "FF wear cannot exceed 5"
  }
  return(ffwear_issues)
}

#'
#'Validate muscle. 2.5,3,4,5, blank allowed. 1 or 2 are allowed but
#'MUST have a note, otherwise it's likely a type-o (check hard copy)
#'
#'@param df bird data frame
#'@return data frame of rows with muscle issues
#'
validate_muscle <- function(df) {
  valid_muscle_list <- c(1, 2, 2.5, 3, 4, 5, NA)
  muscle_issues <- filter(df, !(Muscle %in% valid_muscle_list) | (is.na(NOTES) & (Muscle == 1 | Muscle == 2)))
  if(nrow(muscle_issues) != 0) {
    muscle_issues[, "Issue"] <- "Muscle value invalid. Value must be 2.5 3 4 or 5. 1 and 2 with notes is acceptable"
  }
  return(muscle_issues)
}

#'
#'Validate age and skull combinations. Allowable values for skull 0-6, 8,9,
#'blank. Flag all values in the skull column that don't match these
#'
#'@param df bird data frame
#'@return data frame of rows with age/skull issues
#'
validate_age_skull <- function(df) {
  skull_age_exceptions <- c("HETH", "SWTH", "DEJU", "ORJU", "UDEJ", "HAFL", "DUFL")
  skull_age_issues <- filter(
    df,
    ((SKULL == 1 | SKULL == 2) & !(AGE == 2 | AGE == 4)) | ((SKULL == 3 | SKULL == 4) & AGE != 2)  | (SKULL == 5 & !(SPECIES %in% skull_age_exceptions) & AGE != 2) | (SKULL == 5 & SPECIES %in% skull_age_exceptions & !(AGE == 1 | AGE == 5 | AGE == 6)) | (SKULL == 6 & SPECIES == "RCKI" & !(AGE == 0 | AGE == 2)) | (SKULL == 6 & SPECIES != "RCKI" & !(AGE == 1 | AGE == 5 | AGE == 6))
  )
  if(nrow(skull_age_issues) != 0) {
    skull_age_issues[,"Issue"] <- "Skull-age mismatch"
  }
  return(skull_age_issues)
}

#'
#'Validate how aged and skull combinations
#'
#'@param df bird data frame
#'@return data frame of rows with how aged/skull issues
#'
validate_ha_skull <- function(df) {
  skull_ha_issues <- filter(df, (SKULL == 7 | SKULL == 8) & HA == "SK")
  if(nrow(skull_ha_issues) != 0) {
    skull_ha_issues[,"Issue"] <- "How aged cannot be skull when skull is 7 or 8"
  }
  return(skull_ha_issues)
}

#'
#'Validate location. Make sure there are no blanks
#'
#'@param df bird data frame
#'@return data frame of rows with location issues
#'
validate_location <- function(df) {
  location_issues <- filter(df, is.na(LOCATION))
  if(nrow(location_issues) != 0) {
    location_issues[,"Issue"] <- "Location cannot be blank"
  }
  return(location_issues)
}

#'
#'Validate band size. Make sure there are no blanks. Make sure the only
#'values used are 0A, 0, 1, 1B, 1A, 1C, 2, 3, 3A, 3B
#'
#'@param df bird data frame
#'@return data frame of rows with band size issues
#'
validate_bandsize <- function(df) {
  valid_band_sizes <- c("OA", "O", 1, "1B", "1A", "1C", 2, 3, "3A", "3B", "U", "R")
  band_size_issues <- filter(df, !(BANDSIZE %in% valid_band_sizes))
  if(nrow(band_size_issues) != 0) {
    band_size_issues[,"Issue"] <- "Band size invalid. Acceptable values: 0A 0 1 1B 1A 2 3 3A 3B U R"
  }
  return(band_size_issues)
}

#'
#'Validate band size-disp combinations
#'
#'@param df bird data frame
#'@return data frame of rows with band size/disp issues
#'
validate_bandsize_disp <- function(df) {
  band_size_disp_issues <- filter(df, (BANDSIZE == "U" & DISPOSITION..band.code. != "U") | (BANDSIZE == "R" & DISPOSITION..band.code. != "R"))
  if(nrow(band_size_disp_issues) != 0) {
    band_size_disp_issues[, "Issue"] <- "If band size is U or R, band code must match"
  }
  return(band_size_disp_issues)
}

#'
#'Validate age-ffmolt combinations. Blanks are okay, and can match with any
#'age. Refer to table on rules page
#'
#'@param df bird data frame
#'@return data frame of rows with age/ffmolt issues
#'
validate_age_ffmlt <- function(df) {
  age_ffmolt_exceptions <- c("YBCH", "SPTO", "SOSP", "HOFI", "NOFL", "RSFL", "HAWO")
  valid_ffmolt_156 <- c("N", "S", "A", NA)
  valid_ffmolt_2 <- c("N", "A", "J", NA)
  age_ffmolt_issues <- filter(df,
                              ((AGE == 1 | AGE == 5 | AGE == 6) & !(FF.MLT %in% valid_ffmolt_156)) |
                                (AGE == 2 & !(SPECIES %in% age_ffmolt_exceptions) & !(FF.MLT %in% valid_ffmolt_2)) |
                                (AGE == 4 & !(FF.MLT == "J" | is.na(FF.MLT)))
  )
  if(nrow(age_ffmolt_issues) != 0) {
    age_ffmolt_issues[,"Issue"] <- "Age-ffmolt mismatch"
  }
  return(age_ffmolt_issues)
}

#'
#'Validate how aged-ffmolt combinations.
#'If "how aged" says MR, FF molt must be S or J (can't be blank, N, or A)
#'
#'@param df bird data frame
#'@return data frame of rows with how aged/ffmolt issues
#'
validate_ha_ffmlt <- function(df) {
  ha_ffmolt_issues <- filter(df, HA == "MR", !(FF.MLT == "S" | FF.MLT == "J"))
  if(nrow(ha_ffmolt_issues) != 0) {
    ha_ffmolt_issues[,"Issue"] <- "If how aged is MR then ffmolt must be S or J"
  }
  return(ha_ffmolt_issues)
}

#'
#'Validate how aged-ffwear combinations.
#'If "how aged" says FF then FF Wear cannot be blank
#'
#'@param df bird data frame
#'@return data frame of rows with how aged/ffwear issues
#'
validate_ha_ffwear <- function(df) {
  ha_ffwear_issues <- filter(df, HA == "FF", is.na(FF.WEAR))
  if(nrow(ha_ffwear_issues) != 0) {
    ha_ffwear_issues[,"Issue"] <- "If how aged is FF then ffwear cannot be blank"
  }
  return(ha_ffwear_issues)
}

#'
#'Validate age-ffwear combinations.
#'
#'0 or 1 FF wear is highly suspicious for age 5 and 6. Flag all these records
#'Sometimes 0 FF wear is normal if paired with S FF molt, but then
#'micro-ageing is suspect, so we should flag the record either way, maybe with
#'a message FF wear and age combination unlikely. Check this record
#'
#'2+ FF wear is suspicious for age 4--add message age and FF wear combination
#'unlikely
#'
#'4+ is suspicious for age 2--add unlikely message
#'
#'@param df bird data frame
#'@return data frame of rows with age/ffwear issues
#'
validate_age_ffwear <- function(df) {
  age_ffwear_issues <- filter(df,
                              ((AGE == 5 | AGE == 6) & (FF.WEAR == 0 | FF.WEAR == 1)) |
                                (AGE == 4 & FF.WEAR >= 2) |
                                (AGE == 2 & FF.WEAR >= 4)
  )
  if(nrow(age_ffwear_issues) != 0) {
    age_ffwear_issues[,"Issue"] <- "Age-ffwear mismatch"
  }
  return(age_ffwear_issues)
}

#'
#'Validate wing. Check if wing is below 30 or above 200
#'
#'@param df bird data frame
#'@return data frame of rows with wing issues
#'
validate_wing <- function(df) {
  wing_issues <- filter(df, WING < 30 | WING > 200)
  if(nrow(wing_issues) != 0) {
    wing_issues[,"Issue"] <- "Wing below 30 or above 200 is suspicious"
  }
  return(wing_issues)
}

#'
#'Validate tail. Check if tail is below 30 or above 200
#'
#'@param df bird data frame
#'@return data frame of rows with tail issues
#'
validate_tail <- function(df) {
  tail_issues <- filter(df, TAIL < 30 | TAIL > 200)
  if(nrow(tail_issues) != 0) {
    tail_issues[,"Issue"] <- "Tail below 30 or above 200 is suspicious"
  }
  return(tail_issues)
}

#'
#'Validate weight. Flag anything under 5 but GCKI or BCHU RUHU CAHU okay or over 200 raptors would
#'be a rare exception
#'
#'@param df bird data frame
#'@return data frame of rows with weight issues
#'
validate_weight <- function(df) {
  weight_exceptions_small <- c("RCKI", "BCHU", "RUHU", "CAHU")
  df$WEIGHT <- as.numeric(as.character(df$WEIGHT))
  weight_issues <- filter(df,
                          (WEIGHT < 4 & !(SPECIES %in% weight_exceptions_small)) |
                            (WEIGHT > 200 & SPECIES != "COHA")
  )
  if(nrow(weight_issues) != 0) {
    weight_issues[,"Issue"] <- "Weight is suspicious for non-exception species"
  }
  return(weight_issues)
}

#'
#'Validate band code. Make sure there are no
#'blanks. Make sure the only values used are 1,R,4,5,8,R,U.
#'
#'@param df bird data frame
#'@return data frame of rows with band code issues
#'
validate_bandcode <- function(df) {
  valid_band_code <- c(1, 4, 5, 8, "R", "U")
  band_code_issues <- filter(df, !(DISPOSITION..band.code. %in% valid_band_code))
  if(nrow(band_code_issues) != 0) {
    band_code_issues[,"Issue"] <- "Invalid band code"
  }
  return(band_code_issues)
}

#'
#'Validate band code-species combinations. Make sure 4 and 8
#'are only used for species codes BADE and BALO
#'
#'@param df bird data frame
#'@return data frame of rows with band code/species issues
#'
validate_bandcode_species <- function(df) {
  disp_species_issues <- filter(df, (DISPOSITION..band.code. == 4 | DISPOSITION..band.code. == 8) & !(SPECIES == "BADE" | SPECIES == "BALO"))
  if(nrow(disp_species_issues) != 0) {
    disp_species_issues[,"Issue"] <- "If band code is 4 or 8, band must be destroyed or lost"
  }
  return(disp_species_issues)
}

#'
#'Validate status. Allowable values for new bands: 300, 500. Blank is NOT valid
#'
#'@param df bird data frame
#'@return data frame of rows with status issues
#'
validate_status <- function(df) {
  valid_status <- c(300, 500)
  status_issues <- filter(df, !(STATUS %in% valid_status))
  if(nrow(status_issues) != 0) {
    status_issues[,"Issue"] <- "Invalid status"
  }
  return(status_issues)
}

#'
#'Validate status 500s. ALL status 500's MUST have text in the note column and a letter in the disp
#'column i.e. Note and Disp columns cannot be blank
#'
#'@param df bird data frame
#'@return data frame of rows with status 500 issues
#'
validate_status_500 <- function(df) {
  status_500_issues <- filter(df, STATUS == 500, (is.na(DISP) | is.na(NOTES)))
  if(nrow(status_500_issues) != 0) {
    status_500_issues[,"Issue"] <- "If status is 500, disp and notes cannot be blank"
  }
  return(status_500_issues)
}


#'
#'Validate disp. Allowable values include: M,O,I,S,E,D,T,W,B,L,P, blank
#'
#'@param df bird data frame
#'@return data frame of rows with disp issues
#'
validate_disp <- function(df) {
  valid_disp <- c("M", "O", "I", "S", "E", "D", "T", "W", "B", "L", "P", NA)
  disp_issues <- filter(df, !(DISP %in% valid_disp))
  if(nrow(disp_issues) != 0) {
    disp_issues[,"Issue"] <- "Invalid disp value"
  }
  return(disp_issues)
}

#'
#'Validate disp-status combinations. Any bird
#'with a letter in disp should have a note explaining why and the status should
#'say 500
#'
#'@param df bird data frame
#'@return data frame of rows with disp/status issues
#'
validate_disp_status <- function(df) {
  disp_status_issues <- filter(df, !is.na(DISP), (STATUS != 500 | is.na(NOTES)))
  if(nrow(disp_status_issues) != 0) {
    disp_status_issues[,"Issue"] <- "If disp is not empty, status must be 500 and notes must be filled"
  }
  return(disp_status_issues)
}

#'
#'Validate year. No blanks. Allowable values are any valid year between
#'1997 and current year except BADE BALO
#'
#'@param df bird data frame
#'@return data frame of rows with year issues
#'
validate_year <- function(df) {
  year_issues <- filter(df, YYYY < 1997)
  if(nrow(year_issues) != 0) {
    year_issues[,"Issue"] <- "Year cannot be before 1997"
  }
  return(year_issues)
}

#'
#'Validate year-species combinations
#'
#'@param df bird data frame
#'@return data frame of rows with year/species issues
#'
validate_year_species <- function(df) {
  year_band_issues <- filter(df, is.na(YYYY) & !(SPECIES == "BADE" | SPECIES == "BALO"))
  if(nrow(year_band_issues) != 0) {
    year_band_issues[,"Issue"] <- "Year cannot be blank when band was not lost or destroyed"
  }
  return(year_band_issues)
}

#'
#'Validate month. Valid: 2-11. No blanks except for BADE BALO
#'
#'@param df bird data frame
#'@return data frame of rows with month issues
#'
validate_month <- function(df) {
  month_issues <- filter(df, MM < 2 | MM > 11)
  if(nrow(month_issues) != 0) {
    month_issues[,"Issue"] <- "Month must be between February and November"
  }
  return(month_issues)
}

#'
#'Validate month-species combinations
#'
#'@param df bird data frame
#'@return data frame of rows with month/species issues
#'
validate_month_species <- function(df) {
  month_band_issues <- filter(df, is.na(MM) & !(SPECIES == "BADE" | SPECIES == "BALO"))
  if(nrow(month_band_issues) != 0) {
    month_band_issues[,"Issue"] <- "Month cannot be blank when band was not lost or destroyed"
  }
  return(month_band_issues)
}

#'
#'Validate day. Valid: 1-31. no blanks except for BADE/BALO
#'
#'@param df bird data frame
#'@return data frame of rows with day issues
#'
validate_day <- function(df) {
  day_issues <- filter(df, DD < 1 | DD > 31)
  if(nrow(day_issues) != 0) {
    day_issues[,"Issue"] <- "Day must be between 1 and 31"
  }
  return(day_issues)
}

#'
#'Validate day-species combinations
#'
#'@param df bird data frame
#'@return data frame of rows with day/species issues
#'
validate_day_species <- function(df) {
  day_band_issues <- filter(df, is.na(DD) & !(SPECIES == "BADE" | SPECIES == "BALO"))
  if(nrow(day_band_issues) != 0){
    day_band_issues[,"Issue"] <- "Day cannot be blank when band was not lost or destroyed"
  }
  return(day_band_issues)
}


#'
#'Validate cap time.Allowed values include: 650 to 1300. Flag all
#'other values. Other values may happen only if there is a note, sometimes
#'songbirds are caught during owls, hawk trapping, etc. All values should end
#'in 0's
#'
#'@param df bird data frame
#'@return data frame of rows with cap time issues
#'
validate_captime <- function(df) {
  cap_issues <- filter(df, is.na(NOTES), (CAP.TIME < 650 | CAP.TIME > 1300) | (CAP.TIME %% 10 != 0))
  if(nrow(cap_issues) != 0) {
    cap_issues[,"Issue"] <- "Cap time must end in 0 and be between 650 and 1300"
  }
  return(cap_issues)
}

#'
#'Validate net. Allowable values: 1-12, blank. Some exceptions allowed
#'with a note, e.g. owl nets but we should flag those exceptions anyway to
#'make sure someone checks them
#'
#'@param df bird data frame
#'@return data frame of rows with net issues
#'
validate_net <- function(df) {
  valid_nets <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, NA)
  net_issues <- filter(df, !(NET.. %in% valid_nets))
  if(nrow(net_issues) != 0) {
    net_issues[,"Issue"] <- "Net number must be between 1 and 10"
  }
  return(net_issues)
}

#'
#'Validate notes. Check that notes that mention either, FF, flat flies, or mites, lice,
#'louse, mite, fly have a Y for parasite column
#'
#'@param df bird data frame
#'@return data frame of rows with notes issues
#'
validate_notes <- function(df) {
  note_issues <- filter(df, grepl("ff|flat flies|mites|mite|lice|louse|fly", tolower(NOTES)), Parasites. != "Y")
  if(nrow(note_issues) != 0) {
    note_issues[,"Issue"] <- "If notes mention lice, flies, or mites, parasite column must be Y"
  }
  return(note_issues)
}

#'
#'Validate EY in how aged. EY in the How Aged columns should only be used for species codes SPTO, DOWO,
#'NOFL, RSFL, HAWO, DEJU, ORJU, SCJU, UDEJ --flag any other species that use
#'this with note, Check in Pyle to confirm that this species can be aged by eye
#'color
#'
#'@param df bird data frame
#'@return data frame of rows with EY issues
#'
validate_ey <- function(df) {
  ey_exceptions <- c("SPTO", "DOWO", "NOFL", "RSFL", "HAWO", "DEJU", "ORJU", "SCJU", "UDEJ")
  ey_issues <- filter(df, HA == "EY" | HA2 == "EY", !(SPECIES %in% ey_exceptions))
  if(nrow(ey_issues) != 0) {
    ey_issues[,"Issue"] <- "Check in Pyle to confirm that this species can be aged by eye color"
  }
  return(ey_issues)
}

#'
#'Validate age-how aged combinations
#'
#'@param df bird data frame
#'@return data frame of rows with age/how aged issues
#'
validate_age_ha <- function(df) {
  valid_ha_0 <- c("IC", NA)
  valid_ha_1 <- c("PL", "NL", "EY", "FF", "MB", "PC", "MR", "FB")
  valid_ha2_1 <- c("SK", "TS", NA)
  valid_ha2_5 <- c("PL", "EY", "FF", "MB", "PC", "MR", "SK", "TS", NA)
  valid_ha_2 <- c("PL", "EY", "FF", "MB", "PC", "LP", "MR", "SK", "TS")
  valid_ha2_2 <- c("PL", "EY", "FF", "MB", "PC", "LP", "MR", "SK", "TS", NA)
  valid_ha2_4 <- c("PL", "EY", "FF", "MB", "PC", "LP", "SK", "TS", NA)
  age_ha_issues <- filter(df,
                          (AGE == 0 & !(HA %in% valid_ha_0)) |
                            (AGE == 1 & (!(HA %in% valid_ha_1) | !(HA2 %in% valid_ha2_1))) |
                            (AGE == 2 & (!(HA %in% valid_ha_2) | !(HA2 %in% valid_ha2_2))) |
                            (AGE == 4 & (FF.MLT != "J" | HA != "MR" | !(HA2 %in% valid_ha2_4))) |
                            (AGE == 5 & (HA != "LP" | !(HA2 %in% valid_ha2_5))) |
                            (AGE == 6 & HA != "NL")
  )
  if(nrow(age_ha_issues) != 0) {
    age_ha_issues[,"Issue"] <- "Age-how aged mismatch"
  }
  return(age_ha_issues)
}

#'
#'Validate age-how sexed combinations
#'
#'@param df bird data frame
#'@return data frame of rows with age/how sexed issues
#'
validate_age_hs <- function(df) {
  valid_hs_0 <- c("IC", NA)
  hs_exceptions_0 <- c("RCKI", "GCKI")
  valid_hs_156 <- c("PL", "BP", "CL", "WL", NA)
  valid_hs_2 <- c("PL", "WL")
  age_hs_issues <- filter(df,
                          (AGE == 0 & !(SPECIES %in% hs_exceptions_0) & !(HS %in% valid_hs_0)) |
                            ((AGE == 1 | AGE == 5 | AGE == 6) & !(HS %in% valid_hs_156)) |
                            (AGE == 2 & SEX != "U" & !(HS %in% valid_hs_2))
  )
  if(nrow(age_hs_issues) != 0) {
    age_hs_issues[,"Issue"] <- "Age-how sexed mismatch"
  }
  return(age_hs_issues)
}

#'
#'Validate parasites. If there is a Y in the parasites column there needs to be a note
#'
#'@param df bird data frame
#'@return data frame of rows with parasite column issues
#'
validate_parasites <- function(df) {
  parasite_issues <- filter(df, Parasites. == "Y", is.na(NOTES))
  if(nrow(parasite_issues) != 0) {
    parasite_issues[,"Issue"] <- "If parasites column contains Y notes cannot be blank"
  }
  return(parasite_issues)
}

#'
#'Validate how aged-how aged 2 combinations
#'
#'@param df bird data frame
#'@return data frame of rows with ha/ha2 issues
#'
validate_ha_ha2 <- function(df) {
  haha2_issues <- filter(df, !is.na(HA), !is.na(HA2), as.character(HA) == as.character(HA2))
  if(nrow(haha2_issues) != 0) {
    haha2_issues[,"Issue"] <- "If both how aged columns are filled both cannot be the same"
  }
  return(haha2_issues)
}

#'
#'Validate how sexed-how sexed 2 combinations
#'
#'@param df bird data frame
#'@return data frame of rows with hs/hs2 issues
#'
validate_hs_hs2 <- function(df) {
  hshs2_issues <- filter(df, !is.na(HS), !is.na(HS2), as.character(HS) == as.character(HS2))
  if(nrow(hshs2_issues) != 0) {
    hshs2_issues[,"Issue"] <- "If both how sexed columns are filled both cannot be the same"
  }
  return(hshs2_issues)
}

