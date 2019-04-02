#Data De-Identification Guide algorithm - for scoring Publication ability of Personal Health Information Data
# Criteria taken from DHCS Data De-Identification Guidelines
getDDG <- function (df){
  #Step 1 - Peronsal characteristics of individual records (e.g. Provider Data)
  
  #Step 2 - Denominator & Numerator factoring
  length(tmp_df) >= 20000 | 
     length(tmp_df[tmp_df$RStatus == "Confirmed",]) <= 11
  
  # if(length(tmp_df)
  
  
  #Step 3 - 
  
  #step 4 - Assess potential isk that a small num or den would put individuals at risk...
  # a.Identifiers in CA IPA
  # b.Identifiers in HIPAA
  # c.Demographics\
    
  
  #Step 5 - Publication scoring criteria (score <12 can be released. >12 will require use of statistical masking)
  # events -        {1000+, 100-999, 11-99, <11} {+2, +3, +5, +7}
  index <- c(11, 99, 999)
  value <- c(7, 5, 3, 2)
  # sex -           # of genders {3, 2, 1} {0, 0, 1}
  index <- c(1, 2, 3)
  value <- c(1, 0, 0)
  # age range -     difference of {>10, 6-10, 3-5, 1-2} {2, 3, 5, 7}
  index <- c(2, 5, 10)
  value <- c(7, 5, 3, 2)
  
  # race -          {White, Asian, Black or African American} {2}
  #                 {White, Asian, Black, American Ind, Alaska Native, Native Hawaiian, other Pacific Isl, Mixed} {3}
  #                 {Detailed Race} {4}
  # ethnicity -     {Latino or Hispanic, not Latino} {+2}
  #                 {Detailed ethnicity} {+2}
  #                 {race/ethnicity} {+3}
  
  # language -      {English, Spanish, Other Language} {+2}
  #                 {Single Detailed Language} {+4}
  index <- c(1, 3)
  value = c(4, 2)
  # report period - {>5 year period} {-5}
  #                 {2-4 year period} {-3}
  #                 {1 year period} {0}
  #                 {6 months} {+3}
  #                 {3 months} {+4}
  #                 {1 month} {+5}
  # residence geography
  #                 {Geography with >2,000,000} {-5}
  #                 {1000000 < x <=2000000}{-4}
  #                 {560000 < x <= 10000000} {-3}
  #                 {250000 < x <= 560000} {-1}
  #                 {20000 < x <= 250000}{0}
  #                 {x <= 20000} {+1}
  #                 {x on street} {+3} #May need to focus on zoom level of this mapping
  # service geography - identical to residence
  # variable interactions (at this time, do not have variable interactions (e.g. geography & disease))
  scoreCriteria_df <- data.frame("events"= ,
                              "sex" = ,
                              "ageRange" = ,
                              "race" = ,
                              "ethnicity" = ,
                              "language" = ,
                              "period" = ,
                              "populationResid" = ,
                              "populationServ" = ,
                              "interactions" = )
}

#Notes:
# work on calculating categorical numerators/denominators for selected population still needed
# Provider data...