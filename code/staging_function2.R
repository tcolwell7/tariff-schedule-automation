### Staging function script for HS 2022 schedules

#' v1 - 15/02/22 - Initial update to script for HS 2022 work. 
#'    - 16/02/22 - First run through of code with Norway as test case. Mainly notes column changed to staging and update to commodity_code_cn8 lines. 
#'    - 17/02/22 - Removal of cn8 lines of code as they are to be removed from the schedule. 
#'    - 03/03/22 - staging data changed to v2 following updated source data. 
#' v2 - 03/03/22 - new version saved for extrapolation Function testing. New function created to reduce the code and make it more efficent. 
#'    - 07/03/22 - agg. commodity code QA check added. 
#'    - 11/03/20 - issue spotted with Japan data (mis categorised hs6 code down as a cn8) and removal of columns for rbind. 
#'    - 14/03/22 - av_duty column removed so code will run correctly for Canada and Japan. Column is not removed automatically with Japan so line as been added after row 250
#'    - 24/03/22 - updated for latest staging data v3
#'    - 11/04/22 - updated for latest stagign data v4
#'    
#'    

library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(openxlsx)
library(stringr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

`%notin%` <- Negate(`%in%`) # Custom negate function


staging_data_all <- read_excel("..\\inputs\\staging_data4.xlsx", col_types = "text") %>% clean_names()

# tidy staging data ----

col_names <- staging_data_all %>% select(- country, - commodity_code_long_form, -commodity_code) 
col_names <-  as.data.frame(unique(colnames(col_names))) %>% pull()

# converts all staging data to more useable format
for(x in col_names) {
  
  rate_column <- x
  
  staging_data_all <- staging_data_all %>%
    mutate(
      !!as.symbol(rate_column) := case_when(
        
        grepl("E-3",!!as.symbol(rate_column)) ~
          suppressWarnings(formatC(
            as.numeric(gsub("E-.", "", !!as.symbol(rate_column))) / 1000,
            digits = 4, format = "f"
          )),
        
        grepl("E-",!!as.symbol(rate_column)) ~
          suppressWarnings(formatC(
            as.numeric(gsub("E-.", "", !!as.symbol(rate_column))) / 100,
            digits = 4, format = "f"
          )),
        grepl("00000000|99999999",!!as.symbol(rate_column)) &
          !grepl("E-",!!as.symbol(rate_column)) ~
          suppressWarnings(formatC(
            as.numeric(gsub("E-", "", !!as.symbol(rate_column))),
            digits = 4, format = "f"
          )),
        TRUE ~ !!as.symbol(rate_column)
      )
    )
  
   
}


### convert all data to final format used for notification outputs
staging_data_all_2 <- staging_data_all
for(x in col_names){
  
  new_col_name <- x

  staging_data_all_2$av_duty <- round(as.numeric(unlist(staging_data_all_2[,x]))*100,2)
  
    staging_data_all_2 <- staging_data_all_2 %>% 
      mutate(
        !!sym(new_col_name) := 
          ifelse(
            is.na(av_duty), 
            !!as.symbol(x), 
            as.character(paste0(av_duty, "%"))
            )
    )
}
  



staging_data_all_2 <- 
  staging_data_all_2 %>% 
   mutate(
     commodity_code_long_form = gsub("[.]", "", commodity_code_long_form), 
     commodity_code = gsub("[.]", "", commodity_code),
     commodity_code = gsub("[ *]", "", commodity_code),
     commodity_code_cn8 = substr(commodity_code_long_form, 1, 8)
   ) 

# staging data agg. commodity code long format check:

staging_data_all_2 <-
  staging_data_all_2 %>% 
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


stagingFunction <- function(countryName){
  
  country_name <- countryName
  #country_name <- "Ecuador"
  
  #' staging data and full cn8 codes expansion
  #' filter for specific country
  #' clean data
  #' extend out full cn8/cn10 code listing based on aggregated code list

  staging_data <- staging_data_all_2 %>% filter(country == country_name)
  
  #' apply function to extend schedule:
  staging_data_full <- extrapFunction(staging_data) %>% 
    arrange(final_code) %>%
    mutate(matchID = paste0(final_code, "X")) %>%
    distinct(.,final_code, .keep_all = TRUE)
  
  
  
  data_schedule_staging <- data_schedule_final # main fta_notification script df. 
  
  # need to create unique ID to match so no duplicates:
  
  data_schedule_staging <- 
    data_schedule_staging %>% 
    mutate(matchID = paste0(final_code, staging))
  
  
  data_schedule_staging <- 
    data_schedule_staging %>% 
    left_join(staging_data_full, by = "matchID") %>%
    select(-matchID)
  

  data_schedule_staging <- 
    data_schedule_staging %>% 
    select( - country, 
            - final_code.y, 
            - commodity_code_long_form,
            - commodity_code_cn8, 
            - commodity_code)
  
  # missing codes:
  
  # some codes are missing due to the full schedule - expanding out previous cn8 codes to cn10 where their UKGT is different
  # so methdology doesnt pick these up
  # need to identify missing codes and match in staging info
  #
  # filter missing staging data to match staging in using cn8 code of full cn10
  # cn10 is different because UKGT rate being different at this level
  #
  #
  ## \\ NOTE: for HS 2022 work (Norway) has missing staging data - which this check picks up. \\
  
  # 0302541910
  staging_data_missing <- data_schedule_staging %>% 
    filter(staging == "X" | staging == "X, A") %>%
    filter(is.na(staging_2022)) %>% 
    select(
      fta_country, 
      final_code.x,
      preferential_duty_rate_2022, 
      validity,
      staging,
      notes
      ) %>%
    mutate(commodity_code_cn8 = substr(final_code.x, 1, 8))
  
  
  if(nrow(staging_data_missing) > 0){
  
  # match in staging data (which has been fully extended out to cn8/cn10 level - so all codes shoukd match. If not - there is an issue with the nomeclature)
  staging_data_missing <- staging_data_missing %>% 
    left_join(staging_data_full, by = "commodity_code_cn8")
  
  # check to see if any further missing staging data
  staging_data_missing_2 <- 
    staging_data_missing %>%
    filter(is.na(staging_2022))
  
  
  staging_data_missing <- 
    staging_data_missing %>% 
    filter(final_code.x %notin% staging_data_missing_2$final_code.x)
  
  
  if(nrow(staging_data_missing_2) > 0){
  staging_data_missing_2 <-
    staging_data_missing_2 %>%
    remove_empty(., which = "cols", quiet = TRUE) %>%
    left_join(staging_data_full, by = c("commodity_code_cn8" = "final_code"))
  

  staging_data_missing_2 <- 
    staging_data_missing_2 %>%
    select(
           - country, 
           - commodity_code_long_form, 
           - commodity_code, 
            - matchID,
           -commodity_code_cn8.y,
           -commodity_code_cn8
    )
  
  }
  #remove unnecessary columns to match back into schedule:
  staging_data_missing <- 
    staging_data_missing %>% 
    select(
       - commodity_code_cn8, 
       - country, 
       - commodity_code_long_form, 
       - commodity_code, 
       - final_code, 
       - matchID
      )
  
  
  
  staging_data_missing_3 <- rbind(staging_data_missing, staging_data_missing_2)
  
  #remove staging missing codes from schedule to add back in with staging data - to avoid duplicates
  data_schedule_staging <- 
    data_schedule_staging %>% 
    filter(final_code.x %notin% staging_data_missing_3$final_code.x)
  
  
  data_schedule_staging_final <- 
    rbind(data_schedule_staging, staging_data_missing_3) %>%
    arrange(final_code.x)
  
  } else{
    
    data_schedule_staging_final <- data_schedule_staging
  }
  
  
  # removes columns which are empty - this removes all unnecessary staging columns with no data
  data_schedule_staging_final <- data_schedule_staging_final %>% 
    select(-av_duty) %>%
    remove_empty(., which = "cols", quiet = FALSE) %>% 
    rename(final_code = final_code.x)
  
  
  
# Extend preferential rates -----------------
  
  # extend out pref data to all staging columns if no staging exists:
  ## i.e.  the preferential duty rate 2022 is the staging column if no staging rate exists. 
  
  # need to find staging col names start and end - these are then refereed to to extend out pref rate to none staging codes
  staging_cols <- data.frame(colnames(data_schedule_staging_final)) 
  names(staging_cols)[1] <- "col"
  staging_cols <- filter(staging_cols, grepl("staging", col)) %>% filter(col != "staging")
 # staging_start <- head(staging_cols, 1) %>% pull()
#  staging_end <- tail(staging_cols, 1) %>% pull()
  
  data_schedule_staging_final2 <- 
    data_schedule_staging_final %>%
    mutate(
      pref_av = round(as.numeric(preferential_duty_rate_2022)*100,2),
      pref_rate = 
        ifelse(
          is.na(pref_av), 
          preferential_duty_rate_2022, 
          as.character(paste0(pref_av, "%"))
          )
      )
  
  
  
  for(x in staging_cols$col){
    
    
    data_schedule_staging_final2 <- data_schedule_staging_final2 %>%
      mutate(
        !!sym(x) := 
          ifelse(
            staging == "-",
            pref_rate, 
            !!as.symbol(x)
            )
        )
  }
          
  
  data_schedule_staging_final2 <- data_schedule_staging_final2 %>% select( -pref_av, -pref_rate)
           
  # QA CHECK: **
  # staging QA check: missing data number of codes:
  # if there are missing codes this is due to the data schedule having cn10s where staging is cn8
  # reason being the cn10s are there due to the added UKGT with cn10s to distinguish MFN TRQs
  # for these instances - the missing codes are identified, and matched back in using the original cn8
  # the number of added codes - minus the distinct cn8s should be the same to number of codes in the new data schedule with staging
  # minus the original staging data
  # the following tests that logic:
  
  if(nrow(staging_data_missing) > 0){
  
  staging_original <- 
    staging_data_full %>%
    distinct(final_code) 
  
  staging_original_count <- nrow(staging_original)
  
  staging_codes_missing <- 
    staging_data_missing_3 %>%
    mutate(cn8 = substr(final_code.x, 1, 8))
  
  if(nrow(staging_data_missing_3) != 0){
  staging_codes_missing_dist <- 
    staging_codes_missing %>%
    distinct(cn8)
  
  staging_added_code_count <- (nrow(staging_codes_missing) - nrow(staging_codes_missing_dist))
  
  data_schedule_staging_count <-
    data_schedule_staging_final %>%
    filter(staging == "X" | staging ==  "X, A")
  
  data_schedule_staging_count <- nrow(data_schedule_staging_count)
  
  if(staging_added_code_count != (data_schedule_staging_count - nrow(staging_original))){
    
    message("Error: number of codes with staging is not correct. Please investigate. ")
  }
  
  }
  
  } else{
    
    # if no missing staging data - logic is simple - same number of codes notes as staging should be the 
    # same as the number of staging codes expanded out
    
    staging_count <- 
      data_schedule_final %>%
      filter(staging == "X" | staging ==  "X, A") %>%
      distinct(final_code) %>% pull()
    
    if(nrow(staging_data_full) != length(staging_count)){
      
      message("Staging error: not all codes with staging accounted for")
    }
      
    
}
  return(data_schedule_staging_final2)
}


#' staging data full function is to:
#' extend out fully the aggregated staging data
#' to cn8/cn10 code level
#' This is to provide a complete df
#' to QA the full data schedule against
#' if there are codes with staging in the full schedule
#' but not in this full staging list using the source directly
#' There is an error. 
#'


staging_dataFull <- function(countryName2){
  
  
  country_name <- countryName2
  #country_name <- "Japan"
  
  staging_data <- staging_data_all_2 %>% filter(country == country_name) %>% 
    mutate(commodity_code_long_form = gsub("[.]", "", commodity_code_long_form), 
           commodity_code = gsub("[.]", "", commodity_code),
           commodity_code = gsub("[ *]", "", commodity_code),
           commodity_code_cn8 = substr(commodity_code_long_form, 1, 8))
  
  
  #' Apply extrapFunction to fully extend out cn8/cn10 listing. 
  
  staging_data_full <- extrapFunction(staging_data) %>% 
    arrange(final_code) %>%
    mutate(matchID = paste0(final_code, "X")) %>%
    distinct(.,final_code, .keep_all = TRUE)  %>% 
    remove_empty(., which = "cols", quiet = TRUE)
}