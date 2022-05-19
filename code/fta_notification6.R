# version control -------------------------------------------------------------------

#' v1 - 15/02/2022 - Initiation of converting WTO notifications code to reflect HE 2022 data and scripts. 
#' v2 - 16/02/2022 - Removal of all UKGT references and EPA check. 
#'    - 17/02/2022 - Removal of cn8 code from schedule. Issue has arisen when using new Feb 2022 codes. This can be extracted from the full code at the end of methodology. 
#' v3 - 21/02/2022 - Update to excel automation following change in schedule column formatting and removal of EPA section. 
#' v4 - 04/03/2022 - scheduleFunction script added to compile all functions and reduce the number of scripts. 
#'    - 07/03/2022 - NA descriptions check in schedule formatting function removed (as the input is out of date for 2022).
#'    - 08/03/2022 - Agreement name added back in - and code re-tested and test outputs ran. Excel output automation edited (removal of lines, spaced out, additional setColWidths.)
#' v5 - 08/03/2022 - Excel function added and implemented into excel section. Function moved to scheduleFunctions script. 
#'    - 11/03/2022 - Testing of running all agreements in loop. Amendments to country name/fta country list input for loop. This is to match up all dataset names so the code runs correctly. 
#'                 - Japan staging data issue - need to remove av-duty column which won't affect other countries when run through the function. 
#'    - 14/03/2022 - Staging issue resolved. Test run of Japan and Canada now seems resolved. Further test run of all notifications:
#'                 - Test run: Vietnam name issue affects staging this needs correcting. Otherwise no major issues or errors besides nomenclature not matching which has already been indentified and send to teams to correct. 
#' v6 - 24/03/2022 - updated for latest schedules following update for Master sheet (now apparently final)
#'                 - Outcome: One code for Norway TRQ data not valid. Vitenam name needs fixing so TRQ / Staging data is appended to schedule. 
#'      25/03/2022 - Updated data cleaning to v3
#'      11/04/2022 - Updated master sheet following corrections to Japan/Vietnam/Norway staging and TRQ data. Latest test run:
#' 
#' 

# Setup -------------------------------------------------------------------

rm(list = ls()) # remove items from global environment
# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Specify packages
packages <- c("dplyr", "openxlsx", "janitor", "stringr", "readxl", "tidyverse")

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

`%notin%` <- Negate(`%in%`) # Custom negate function

source("scheduleFunctions.R")
source("data_cleaning3.R") # load in cleaned codes, FTA and UKGT data
source("staging_function2.R") # load in staging data and function to append data to schedule
source("trq_data_function3.R") # load in TRQ data and function to append data to schedule

# 0. Filter countries -----

#' user input for selecting countries / agreements to build schedules for:

countryFilt <- c("Colombia","Ecuador","Peru","Chile","Mexico","Albania")
  
country_input_list <- country_input %>% filter(country_name %in% countryFilt)

#' OR filter based on agreement:

agreementFilt <- c("South Africa Customs Union plus Mozambique","Andean countries")

country_input_list <- country_input %>% filter(agreement_name %notin% agreementFilt)


# 1. Creating full schedule tables ---------------

# Creating list to append data for each country into
data_list <- list()
trq_data_list <- list()
consolidated_list <- list()
na_list <- list()

# need to create unique name for each FTA country and add into FTA preferences. 
# Then one single name can filter the various staigng/TRQdata/imports etc

# Looping over each country

for (country in country_input_list$fta_country_list){
 
 # filter agreement name for output: 
 agreement_name_for_output <- country_input_list %>%
   filter(fta_country_list == country) %>%
   select(agreement_name) %>% 
   mutate(agreement_name = paste0("UK-",agreement_name)) %>% 
   pull()
 
  
  # filter data for selected country
   
  print(paste0("Creating schedule for **************",country,"***************"))
  
  fta_data <- fta %>% 
    filter(fta_country == all_of(country)) 
  
  # concatonate validity dates together with pref rates function:
  data <- fta_validityConcatFunction(fta_data)
  
  ## 1.1 Concatonate code check ------
  # checks number of distinct codes aggregated and number of rows removed from fta data
  concat_code_check <-
    fta_data %>%
    filter(validity != "-") %>%
    group_by(commodity_code_long_form) %>%
    summarise(code_count = length(commodity_code_long_form)) %>% 
    mutate(code_count_reduced = code_count - 1)
  
  data_concat <- (nrow(fta_data) - nrow(data))

if(nrow(concat_code_check) > 0){  
  
  if(sum(concat_code_check$code_count_reduced) == data_concat){
    
    
    print(paste0(nrow(concat_code_check), " distinct commoditiy codes with validity rates and ",
                 data_concat, " rows removed for ", country))
    
  } else{
    
    message("Not all validity rates concatonated")
  }
    
}  
  
  ### 1.2 Extend out schedule -----------------------------------
  
  #' apply extrapolation function extending out full schedule
  #' include all cn10/8 codes which are "missing"
  #' or "excluded
  #' i.e. cn10 codes which dont have preferential rates applied
  
  data_schedule <- extrapFunction(data, .exc = "Y") %>% arrange(final_code)
  
  
#### 1.3 Missing codes ------------------------------------------------
  
  #' Finding/adding commodity codes which are not given a pref rate
  #' but are in the full eu nomenclature list 
  
  missing_codes <- eu_codes %>%
    filter(cn8 %notin% data_schedule$final_code &
             cn10 %notin% data_schedule$final_code) 
  
  # If these missing codes have multiple cn10 under cn8 then keep all cn10, otherwise aggregate to cn8
  missing_codes <- missing_codes %>% 
    group_by(cn8) %>% 
    mutate(cn_consolidated = ifelse(n_distinct(cn10) > 1, "cn10", "cn8")) %>% 
    ungroup() %>% 
    mutate(final_code = ifelse(cn_consolidated == "cn10", cn10, cn8)) %>% 
    distinct(final_code, .keep_all = TRUE)
  
  
  # Tidying to match format of data_schedule
  missing_codes <- missing_codes %>% 
    select(final_code, 
           commodity_code_long_form = cn10) %>%
    mutate(
      fta_country = all_of(country),
      commodity_code_cn8 = str_sub(commodity_code_long_form, 1, 8),
      commodity_code = final_code,
      preferential_duty_rate_2022 = "excluded",
      validity = "-",
      staging = "-",
      notes = "-"
    )
  
  
  # Combine with data schedule to create list of all codes
  data_schedule <- rbind(data_schedule, missing_codes) %>% arrange(final_code)
  
  
### Data schedule output ---------------------------------------------------------
  
  # Tidying columns in final version 
  data_schedule_final <- data_schedule %>% 
    select(
      fta_country, 
           final_code, 
           preferential_duty_rate_2022,
           validity, 
           staging, 
           notes
      )
  
   
# 2. Staging ----------------------------------------------------------------------
   
   # apply staging function 
   
     #staging_countries <- staging_data_all %>% distinct(., country) %>% pull()
       
     staging_country <- 
      country_input_list %>% 
      filter(fta_country_list == country) %>% 
      select(staging_country_list) %>% pull()
  
     if(staging_country %in% unique(staging_data_all$country)){
       
       print(paste0("STAGING: Appending staging data for"," ",staging_country))
       
       data_schedule_final_staging <- stagingFunction(staging_country)
       
       data_schedule_final <- data_schedule_final_staging
       
       #' final QA check to ensure all staging codes are accounted for in schedule
       #' filter data schedule based on codes given in staging data
       #' dataframes should be blank as all codes are in notification schedule.
      
#### Staging final QA ---------------------------------------------
       staging_data_qa <- staging_dataFull(staging_country) 
       
       data_schedule_final_staging_qa <-
         data_schedule_final_staging %>%
         mutate(cn8 = substr(final_code, 1,8)) 
       
       staging_data_qa_cn8 <-
         staging_data_qa %>% 
         filter(str_length(final_code) == 8) %>%
         filter(final_code %notin% data_schedule_final_staging_qa$cn8)
       
       staging_data_qa_cn10 <-
         staging_data_qa %>%
         filter(str_length(final_code) == 10) %>%
         filter(final_code %notin% data_schedule_final_staging_qa$final_code)
       
       staging_cn8_check  <- nrow(staging_data_qa_cn8)
       staging_cn10_check <- nrow(staging_data_qa_cn10)
       
       if(staging_cn8_check > 0 | staging_cn10_check > 0){
         
         message("Not all staging data is accounted for - please investigate")
       } else{
         
         print("STAGING FINAL QA CHECK PASSED: all staging rates accounted for")
            }
       
     } else{
       
       print(paste0("STAGING: No staging informaation for"," ",country))
     }
   
# 3. TRQ data ---- 
   
  
   #countryName <- country
   
 # countryName <- "Canada"
   trq_countryName <- country_input_list %>% filter(fta_country_list == country) %>%
     select(trq_country_list) %>% pull()

   #trq_countryName <- country
   
   if(trq_countryName %in% unique(trq_data_all$trq_country)){
     
     print(paste0("TRQ DATA: Appending TRQ data for"," ",trq_countryName))
     
     # data_schedule_final2 is refered to in trqDataFunction
    data_schedule_final_trq <- trqDataFunction(trq_countryName)
    #data_schedule_final_trq <- data_schedule_trq
   
     # create TRQ full dataframe to include in excel output:
     trq_data_output <- trqDataLongFormat_function(trq_countryName)
     
     trq_data_list[[country]] <- trq_data_output
     
     # create distinct trq code list
     
     trq_data_distinct <- 
       trq_data_output %>%
       distinct(., `Commodity code`, .keep_all = TRUE) %>%
       mutate(cn8 = substr(`Commodity code`, 1, 8)) %>%
       select(`Commodity code`, cn8)
     
     # attach TRQ data tariff line codes to schedule:
     
     data_schedule_final_trq2 <- 
       data_schedule_final_trq %>%
       mutate(
         cn8 = substr(final_code, 1, 8)
         ) %>%
       left_join(
         trq_data_distinct, 
         by = c("cn8" = "Commodity code")
         ) %>%
       mutate(
         trq_code = 
           ifelse(
             !is.na(quota_number_concat) & is.na(cn8.y), 
             final_code,
            ifelse(
             !is.na(quota_number_concat) & !is.na(cn8.y),
             cn8, 
             "n/a")
           )
         ) %>%
       select(-cn8, - cn8.y)
     
     
### TRQ final QA ------------------------------------
     
     # QA check to determine if any duplicates have been casued when matching TRQ tariff line codes
     qa_check <- nrow(data_schedule_final_trq)
     qa_check2 <- nrow(data_schedule_final_trq2)
     
     if(!(qa_check == qa_check2)){
       
       message("Error matching TRQ tariff lines, duplicate codes created")
     }
     
     # schedule following TRQ matched in
     data_schedule_final2 <- data_schedule_final_trq2
     
     # TRQ data QA check: all codes accounted for in schedule
     
     data_schedule_final_trq_qa <- 
       data_schedule_final_trq %>% 
       mutate(cn8 = substr(final_code, 1, 8)) %>%
       filter(!is.na(quota_number_concat))
     
     # check that all cn8 codes in TRQ data appear in final schedule (filtered for TRQ data only)
     trq_data_output_cn8  <- 
       trq_data_output %>% 
       filter(str_length(`Commodity code`) == 8) %>%
       mutate(code_in_schedule = 
                ifelse(`Commodity code` %in% data_schedule_final_trq_qa$cn8, 0, 1))
      
     # check all cn10s in TRQ data are in final schedule (filtered for TRQ data only) 
     trq_data_output_cn10 <- 
       trq_data_output %>% 
       filter(str_length(`Commodity code`) == 10) %>%
       mutate(code_in_schedule = 
                ifelse(`Commodity code` %in% data_schedule_final_trq_qa$final_code, 0, 1))
     
     if(sum(trq_data_output_cn10$code_in_schedule) > 0 |
            sum(trq_data_output_cn8$code_in_schedule) > 0){
       
       message(paste0("Final TRQ data check for ", country, 
                      ": missing TRQ data - please investigate"))
     } else{
       
       print(paste0("FINAL TRQ DATA CHECK: All commodity codes accounted for"))
     }
     
     
   } else{
     
     print(paste0("No TRQ data for"," ",country))
     data_schedule_final2 <- data_schedule_final
   }
   
  
# 4. Schedule Output ------------------------------------------------------------------
  
   
   data_schedule_final3 <- data_schedule_final2 %>% 
     mutate(hs2 = substr(final_code, 1, 2)) %>%
     left_join(commodity_headings, by = "hs2") %>%
     relocate(., commodity_heading,  .before = final_code) %>%
     select( -hs2)
   
### cn10 consolidation -----
   
   # consolidate cn10s to cn8 where no varying UKGT/pref/TRQ rates
   
   data_schedule_final4 <- cn10_consolidationFunction(data_schedule_final3)
   
   
   # consolidated code outputs
   
   codes_consolidated <- data_schedule_final3 %>% 
     filter(final_code %notin% data_schedule_final4$final_code) %>%
     mutate(cn8 = substr(final_code, 1, 8))
   
   consolidated_list[[country]] <- codes_consolidated
   
   
   consolidated_codes <- 
     data_schedule_final3 %>% 
     filter(final_code %notin% data_schedule_final4$final_code) %>%
     mutate(cn8 = substr(final_code, 1,8)) %>%
     distinct(cn8) %>% pull()
   
   print(paste0(nrow(data_schedule_final3) - nrow(data_schedule_final4), 
                " cn10 rows consolidated to cn8 where all underlying cn10 rates are the same across ",
                length(consolidated_codes), " cn8 codes"))
   
   
   # duplicate code check:
   
   dup_code <- data_schedule_final4 %>% 
     group_by(final_code) %>%
     summarise(code_count = length(final_code), .groups = 'drop') %>%
     filter(code_count > 1)
   
   nrows <- nrow(dup_code)
   
   if(!(nrows == 0)){
     
     message("QA CHECK; DUPLICATE CODE CHECK: FAILED FOR ", country)
     
   } 
   
#### Final formatting for output -----
   
   data_schedule_final5 <- scheduleFormatting(data_schedule_final4)
   
   # match HS 2022 descriptions is NA:
   

   data_schedule_final6 <-
     data_schedule_final5 %>%
     left_join(eu_codes_dsc[,c("cn10", "name")], by = c("Commodity code" = "cn10")) %>%
     left_join(eu_codes_dsc_cn8[,c("cn8","name")], by = c("Commodity code" = "cn8")) %>%
     mutate(
      `Commodity code description` =
        ifelse(
         `Commodity code description` == "n/a" &
           `8-digit or 10-digit` == 10,
         name.x,
        ifelse(
          `Commodity code description` == "n/a" &
                 `8-digit or 10-digit` == 8,
               name.y,
               `Commodity code description`))
     ) %>%
     select(-name.x, -name.y)

   na_descriptions <- data_schedule_final6 %>%
     filter(`Commodity code description` == "n/a")
   
   data_list[[country]] <- data_schedule_final6
   
   
   if(!(nrow(na_descriptions) == 0)){
     
     message(paste0("Final NA check: Not all commodity codes have self-contained descriptions for ", country))
   }
   
   na_list[[country]] <- na_descriptions
   
   
   print(paste0("FINAL: Schedule created for ", country, " with ",
                nrow(data_schedule_final6), " rows"))
   
} # end bracket for goods schedule loop

na_codes_list <- bind_rows(na_list)



# 6. Excel Output ----------------------------------------------

# filter based on FTA name 
for (.country in unique(country_input_list$fta_country_list)){

# extract data from lists to export into excel 
  
  # filter name for output:
  
  countryName <- country_input_list %>% filter(fta_country_list == .country) %>% pull()

  df_goods <- data_list[[.country]]
  df_trq_data <- trq_data_list[[.country]]
  
  # enter inputs into automated excel function
  excel_wbFunction(df_goods, df_trq_data, countryName)
  
    
} # for loop bracket end 


#end.



