# TRQ data_function for HS 2022

#' v1 - 16/02/2022 - Initiation of updating TRQ function for HS 2022 work. Created Data agg function to remvoe repeated lines of code for aggregation level sof long formatted codes. 
#'    - 03/03/2022 - trq data input changed ot v2 to reflect latest master data provided. 
#'    - 04/02/2022 - cleaning of code. Removal of format tariffs - inclusion of source script instead to reduce script size. 
#' v2 - 05/02/2022 - Inclusion of extrapFunction to reduce size of script. 
#' v3 - 07/03/2022 - commodity_code changed to commodity_code and QA check added to capture agg. commoidty codes which are in long format (10 digit when the code should be 6 or 8 digits)
#'    - 24/03/2022 - updated for v3 of TRQ data. fta_countey column used to filter data chnaged to trq_country to streamline country inputs across scripts. 
#'    - 11/04/2022 - updated for v4 TRQ data to reflect Norway updated code.
#'    

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(openxlsx)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set file path

source("scheduleFunctions.R") # load in format tariff function

`%notin%` <- Negate(`%in%`) # Custom negate function

# 0.1 load and tidy TRQ data -----------

trq_data_raw <- read_excel("..\\inputs\\trq_data4.xlsx", col_types ="text") %>% clean_names()

# TRQ data slimed down to attached to schedules
trq_data_all <- 
  trq_data_raw %>%
  mutate(
    commodity_code = 
      gsub("[.]", "", commodity_code),
    commodity_code_cn8 = 
      substr(commodity_code, 1, 8)
    ) %>%
  relocate(
    .,
    commodity_code_cn8, 
    .after = commodity_code
    )



trq_data_all   <- format_tariff_rates(trq_data_all, "quota_duty_rate")

#' TRQ agg. commoidty_code QA check:
#' Some aggregated codes (i.e. 6 or 8 digits) are in the long format (10 digits)
#' This causes an issue when extrapolating the data. 
#' Rather than htis manually to be changed when found
#' automated QA check and update added within code
#' 


trq_data_all <-
  trq_data_all %>% 
  mutate(str_r = str_sub(commodity_code, 9,10)) %>%
  mutate(
    commodity_code =
      ifelse(
        str_r == "00",
        str_sub(commodity_code,1,8),
        commodity_code
      )
  ) %>%
  select(-str_r)

## 0.2 commodity code aggregation level  -----

trq_aggFunction <- function(data){

data2 <- data %>% 
  mutate(
    commodity_hs2 = substr(commodity_code, 3, 10),
    commodity_hs4 = substr(commodity_code, 5, 10),
    commodity_hs6 = substr(commodity_code, 7, 10),
    commodity_hs8 = substr(commodity_code, 9, 10)
    )

# counts number of zeros in each code and identifies the hs aggregations 
data2 <- data2 %>% 
  mutate(
    hs2_zero = str_count(commodity_hs2, "0"),
    hs4_zero = str_count(commodity_hs4, "0"),
    hs6_zero = str_count(commodity_hs6, "0"),
    hs8_zero = str_count(commodity_hs8, "0")
    )


data2 <- data2 %>% 
  mutate(
    commodity_code_agg = 
      ifelse(
        hs2_zero == 8, 
         "hs2_zero", 
      ifelse(
        hs4_zero == 6, 
         "hs4_zero",
      ifelse(
        hs6_zero == 4, 
         "hs6_zero",
      ifelse(
        hs8_zero == 2, 
        "hs8_zero",
        "none")
      )
    )
  )
)


## \\\ NOTE: HS 2022 data there is significantly less codes in "long format" 
##           so above methodology only captures a few codes. 
##           Where as previous data in 2021 had far larger numbers of long formatted codes. \\\ ##


# compiling final TARIC pref. rate aggregations:
data2 <- data2 %>% 
  mutate(
  commodity_code = 
    ifelse(
      commodity_code_agg == "hs2_zero", 
       substr(commodity_code, 1, 2),
    ifelse(
      commodity_code_agg == "hs4_zero", 
       substr(commodity_code, 1, 4),
    ifelse(
      commodity_code_agg == "hs6_zero", 
       substr(commodity_code, 1, 6),
    ifelse(
      commodity_code_agg == "hs8_zero", 
       substr(commodity_code, 1, 8),
        commodity_code)
   )
  )
 )
)




data2 <- data2 %>% 
  select(!(commodity_hs2:commodity_code_agg)) %>%
  relocate(commodity_code, .after = commodity_code_cn8) %>%
  relocate(trq_country, .after = fta_country)

}

trq_data_all  <- trq_aggFunction(trq_data_all)

trq_data_all2 <- 
  trq_data_all %>% 
  select(
    fta_country, 
    trq_country,
    quota_number,
    origin_quota,
    commodity_code,
    commodity_code_cn8,
    commodity_code,
    quota_duty_rate
    )

## Functions: ----------


trqDataFunction <- function(trq_countryName){
  
   #trq_countryName <- "Norway"
   #trq_data <- trq_data_all %>% filter(trq_country == trq_countryName)
  trq_data <- trq_data_all2 %>% filter(trq_country == trq_countryName)
  
  trq_data_full <- extrapFunction(trq_data) %>% 
    arrange(final_code) 
  

  #' There are multiple codes with duplicate rows
  #' due to different quota's with the same commodity code - 
  #' these need to be in "wide format"
  #' need count of commodity code:
  
  #' commodity code count > if greater than 1 
  #' then the commodity code has a quota rate across different quotas
  #' Method: group by data to count number of instances of commodity code
  #' 
  trq_data_count <- 
    trq_data_full %>% 
    group_by(final_code, quota_number) %>%
    summarise(code_count = length(final_code), .groups = 'drop')
  
  #' Method: if the quota count is > 1 - 
  #' this means there are different quotas for each code
  #' and both quotas need to be displayed
  trq_rate_count <- 
    trq_data_count %>% 
    group_by(final_code) %>%
    summarise(quota_count = length(final_code), .groups = 'drop')
  
  
  # creates a unique identifier based on a commodity codes quota rates, if a commodity has the same rate across quotas
  # this code is reduced to a single row 
  trq_data_final <- 
    trq_data_full %>% 
    select(
      final_code, 
      quota_number, 
      origin_quota, 
      quota_duty_rate
      ) %>% 
    left_join(trq_rate_count, by = "final_code") %>%
    mutate(rowID = paste0(final_code, "-",quota_number,"-", quota_count)) %>%
    distinct(., rowID, .keep_all = TRUE)
  
  trq_data_final <- trq_data_final %>% 
    select(
      final_code, 
      quota_number
      ) %>%
    group_by(final_code) %>%
    mutate(
      id = 1:n(),
      col_id = paste0("quota_number","_",id)
      ) %>%
    select( - id)
  
  trq_data_final_wide <- 
    trq_data_final %>% 
    pivot_wider(names_from = "col_id", values_from = "quota_number")
  
  trq_wide_col <- ncol(trq_data_final_wide)
  
  ## concatenate into one cell where a code is under multiple quotas
  
  trq_data_final_wide[is.na(trq_data_final_wide)] <- ""
  
  # combine wide format columns
  # remove " ; "
  # doesn't remove fully when 3 strings atatched - need to remove white space
  trq_data_final_wide <-
    trq_data_final_wide %>%
    unite("quota_number_concat", 2:trq_wide_col, sep = " ; " , remove = FALSE) %>%
    mutate(
      quota_number_concat = 
        ifelse(
          str_length(quota_number_concat) >= 16, 
          quota_number_concat,
          str_remove(quota_number_concat, " ; "))
      ) %>%
    select(final_code, quota_number_concat)
  
  # remove " ; " from rows with one quota
  trq_data_final_wide <- 
    trq_data_final_wide %>%
    mutate(
      quota_number_concat = 
        ifelse(
          str_length(quota_number_concat) >= 12, 
          quota_number_concat,
          str_remove(quota_number_concat, " ; ")
          )
      )
  
  # final removal for when there are three quotas concatonated together in a countries data:
  # when two quotas the pattern isn't reconginsed. 
  # can remove using "LEFT" function equivalent 
  trq_data_final_wide <- 
    trq_data_final_wide %>%
    mutate(
      str_len = 
        str_length(quota_number_concat)
      ) %>%
    mutate(
      quota_number_concat =  
        str_trim(quota_number_concat, side = "right")
      ) %>%
    mutate(
      quota_number_concat = 
        ifelse(
          str_len == 20, 
          substr(quota_number_concat, 1, 18),
          quota_number_concat
          )
      ) %>%
    select( - str_len)
  

  data_schedule_trq <- data_schedule_final
  
  data_schedule_trq <- data_schedule_trq %>% left_join(trq_data_final_wide, by = "final_code")
  

  
#   # QA check if all TRQ data in:
#   
  trq_data_count <- data_schedule_trq %>% filter(!is.na(quota_number_concat))

  if(nrow(trq_data_count) < nrow(trq_data_final_wide)){

    print("TRQ DATA CHECK: Not all TRQ data is in schedule")

    # missing cn10s TRQ data -----------

    # some TRQ data is given at cn10 - when the expanded out codes in the schedule are at cn8.
    # some TRQ datais given at cn8 - but these cn8 codes in the schedule are expanded out at cn10
    # this means some TRQ codes are not matched into the schedule
    # the following identifies codes and binds them back in - keeping in the original cn8 and additional cn10s
    # only if all TRQ data isn't accounted for in the schedule


    trq_data_missing <- trq_data_final_wide %>%
      filter(final_code %notin% data_schedule_trq$final_code) %>%
      mutate(cn8 = substr(final_code, 1, 8))

    
    # this identifies which cn8 codes's underlying cn10s are missing from the TRQ data
    data_schedule_trq_missing_cn10 <- 
      data_schedule_trq %>%
      filter(final_code %in% trq_data_missing$cn8) %>%
      #mutate(trq = ifelse(is.na(trq), "n/a", trq), # add in filled columns so these are not removed for future rbind
       #      mfn_in_quota_rate = ifelse(is.na(mfn_in_quota_rate), "n/a", mfn_in_quota_rate)) %>%
      remove_empty(., which = "cols", quiet = TRUE)  # removes empty cols as they will be matched back in
      
      
      if("quota_number_concat" %in% colnames(data_schedule_trq_missing_cn10)){
  
        message(paste0("TRQ duplicate data check: Duplicate cn8 and cn10 given in TRQ data for ",
                       country," - please investigate"))
      }
    
    # identifies which cn10s have cn8 TRQ data 
    data_schedule_trq_missing_cn8 <- 
      data_schedule_trq %>%
      mutate(cn8 = substr(final_code, 1, 8)) %>%
      filter(cn8 %in% trq_data_missing$final_code) %>%
      filter(is.na(quota_number_concat)) %>%
     # mutate(trq = ifelse(is.na(trq), "n/a", trq),
      #       mfn_in_quota_rate = ifelse(is.na(mfn_in_quota_rate), "n/a", mfn_in_quota_rate)) %>%
      remove_empty(., which = "cols", quiet = TRUE) 
    

if(nrow(data_schedule_trq_missing_cn10) == 0){
  
  print("TRQ DATA CHECK: Identifying missing cn8 TRQ rates")
  
      # match in missing TRQ data to cn10 codes identified as having cn8 codes with TRQ data
      trq_data_missing_cn8 <- data_schedule_trq_missing_cn8 %>% 
        left_join(trq_data_missing, by = "cn8") %>%
        select(-cn8, - final_code.y) %>%
        rename(final_code = final_code.x)
        
      #filter final_codes to rbind back in with final TRQ rates
      data_schedule_trq <- data_schedule_trq %>% 
        filter(final_code %notin% trq_data_missing_cn8$final_code)
      
      # rbind doesn't work when staging columns are blank 
      # bind_rows combined the data regardless of matching columns
      
      #data_schedule_trq$trq <- as.character(data_schedule_trq$trq)
      
      #trq_data_missing_cn8$trq <- as.character(trq_data_missing_cn8$trq)
      
      data_schedule_trq <- bind_rows(data_schedule_trq, trq_data_missing_cn8) %>%
        arrange(final_code)
       
     #return(data_schedule_trq)
} else if(nrow(data_schedule_trq_missing_cn8) == 0){
      
 print("TRQ DATA CHECK: Identifying missing cn10 TRQ rates")
      
       
      # if missing cn10 codes - want to include all codes under the parent cn8, regardless of TRQ data
      # this is inline with FTA pref data methodology
  
      eu_codes_missing_cn10 <- eu_codes %>% 
        filter(cn8 %in% data_schedule_trq_missing_cn10$final_code) %>%
        select(cn10, cn8)
      
      trq_data_missing_cn10 <- eu_codes_missing_cn10 %>%
        left_join(data_schedule_trq_missing_cn10, by = c("cn8" = "final_code")) %>%
        left_join(trq_data_missing, by = c("cn10" = "final_code")) %>%
        rename(final_code = cn10) %>%
        select(-cn8.x , -cn8.y)
        
  
      # remove previous final code cn8 - to add bakc in full listing of cn10s
      
      data_schedule_trq <- data_schedule_trq %>% 
        filter(final_code %notin% data_schedule_trq_missing_cn10$final_code)
      
      
      #data_schedule_trq$trq <- as.character(data_schedule_trq$trq)
      
      #trq_data_missing_cn10$trq <- as.character(trq_data_missing_cn10$trq)
      
      data_schedule_trq <-   bind_rows(data_schedule_trq, trq_data_missing_cn10) %>%
        arrange(final_code)
      
      
      #return(data_schedule_trq)
      
    } else {

      print("TRQ DATA CHECK: Identifying missing cn10 and cn8 codes")

      # cn8s --
      
      trq_data_missing_cn8 <- 
        data_schedule_trq_missing_cn8 %>% 
        left_join(trq_data_missing, by = "cn8") %>%
        select(-cn8, - final_code.y) %>%
        rename(final_code = final_code.x)
      
      #filter final_codes to rbind back in with final TRQ rates
      data_schedule_trq <- 
        data_schedule_trq %>% 
        filter(final_code %notin% trq_data_missing_cn8$final_code)
      
      #data_schedule_trq$trq <- as.character(data_schedule_trq$trq)
      
      #trq_data_missing_cn8$trq <- as.character(trq_data_missing_cn8$trq)
      
      data_schedule_trq <- bind_rows(data_schedule_trq, trq_data_missing_cn8) %>%
        arrange(final_code)
      
      # cn10s --
      
      eu_codes_missing_cn10 <- eu_codes %>% 
        filter(cn8 %in% data_schedule_trq_missing_cn10$final_code) %>%
        select(cn10, cn8)
      
      trq_data_missing_cn10 <- eu_codes_missing_cn10 %>%
        left_join(data_schedule_trq_missing_cn10, by = c("cn8" = "final_code")) %>%
        left_join(trq_data_missing, by = c("cn10" = "final_code")) %>%
        rename(final_code = cn10) %>%
        select(-cn8.x , -cn8.y)
      
     #trq_data_missing_cn10$trq <- as.character(trq_data_missing_cn10$trq)
      
      data_schedule_trq <- data_schedule_trq %>% 
        filter(final_code %notin% data_schedule_trq_missing_cn10$final_code)
      
      
      data_schedule_trq <-   bind_rows(data_schedule_trq, trq_data_missing_cn10) %>%
        arrange(final_code)
      
     # return(data_schedule_trq)
     

} # else end bracket 
      
 
   return(data_schedule_trq)
    data_schedule_trq_final <- data_schedule_trq
    
 #end original if bracket to match bakc in missing TRQ rates

}  
 
  return(data_schedule_trq) 
} # end function bracket



trqDataLongFormat_function <- function(trq_countryName){
  
  

  
  trq_data__ <- trq_data_all %>% filter(trq_country == trq_countryName)
  
  trq_data_ <- trq_validityConcatFunction(trq_data__)
  
  #trq_data_ <- trq_data__
  
  trq_data_full_ <- extrapFunction(trq_data_) %>% 
    arrange(final_code) %>%
    select(`Partner` = trq_country, 
           `Quota number` = quota_number,
           `Origin Quota` = origin_quota,
           `Commodity code` = final_code,
           `In quota duty` = quota_duty_rate,
           `Quota volume` = quota_volume,
           `Quota period` = quota_period,
           `Quota unit of measurement` = units,
           `Quota increase` = quota_increase,
           `Notes` = notes
           )
  
  # convert in quota rates to data schedule output formatting  
  
  trq_data_full_ <-
    trq_data_full_ %>%
    mutate(
      av_duty = round(as.numeric(`In quota duty`)*100,2),
      `In quota duty` = 
        ifelse(
          is.na(av_duty), 
           `In quota duty`,
            as.character(paste0(av_duty,"%"))
          )
      ) %>%
    select(-av_duty) %>%
    mutate(
      `Quota period` = 
        ifelse(
          `Quota period` == "CY", 
          "Calendar year", 
          `Quota period`)
      )
  
  
  return(trq_data_full_)
}
   