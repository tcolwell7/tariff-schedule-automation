## concatenation Functions HS 2022

#' v1 - 15/02/2022 - First run through of code functions for HS 2022 data. Function can be heavily reduced using group by to concat into a single cell rather than a loop. 
#' v1 - 04/03/2022 - Script name changed to scheduleFunctions - all common functions saved in this script to reduce size of prject i.e. number of scripts and reduce repeat lines. 
#'    - 04/03/2022 - Script cleaning. trq_validityConcat function needs to be reduced. tbc. 
#'    - 08/03/2022 - Agreement name added back into schedule - adjusting relocate lines within scheduleFormatting function. 
#'    - 08/03/2022 - Excel workbook function added. 
#'    - 24/03/2022 - output folder for scheduels updated to v6. TRQ country Name added to schedule format function to streamline names across scripts. 
#'    -




# Format tariff rates ------------------
## description ----------------------------




format_tariff_rates <- function(data, rate_column) {
  
  # Detects where precision error has occurred 
  # Transforms to numeric for correct rounding/signif level
  data <- data %>%
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
  return(data)
}









# FTA validity concatenation ------------
## description --------------------------

#' concatenation function identifies which commodity codes are subject to validity
#' which is expressed across multiple rows. 
#' for outputs these codes are to have their rates and 
#' validity concatenated into one cell
#' the following function runs through each code, 
#' creates a wide data frame for each code and automates the concatenation
#' regardless of how many rows are required to be pulled into one
#' once these concatenated codes have been pulled together, 
#' they are appended back into the main FTA data
#' 


fta_validityConcatFunction <- function(data){

  
fta_validity <- data %>%
  filter(validity != "-") %>%
  mutate(
    pref_av_duty = round(as.numeric(preferential_duty_rate_2022)*100,2),
    preferential_duty_rate = 
      ifelse(
        is.na(pref_av_duty),
        preferential_duty_rate_2022,
        as.character(paste0(pref_av_duty, "%"))
        )
    ) %>%
  mutate(
    preferential_duty_rate_validity = 
      paste0(preferential_duty_rate, " ( ", validity, " )")
    )


#' Logic: 
#' if a country has no validity data - 
#' the function just returns what is entered (data2). 
#' Otherwise the data/codes with validity is run through the loop

if(nrow(fta_validity) >= 1){
  
  
  fta_validity2 <- fta_validity %>%
    group_by(commodity_code_long_form) %>%
    summarise(
      preferential_duty_rate_concat = 
        paste0(preferential_duty_rate_validity, collapse = " ; ")
      )
      
  

fta_validity3 <- fta_validity %>% 
  left_join(fta_validity2, by = "commodity_code_long_form") %>%
  distinct(., commodity_code_long_form, .keep_all = TRUE) %>%
  select(
         fta_country, 
         commodity_code_long_form,
         commodity_code,
         commodity_code_cn8, 
         preferential_duty_rate_2022 = preferential_duty_rate_concat,
         validity,
         staging,
         notes
  ) 
       
fta_2 <- 
  data %>% 
  filter(commodity_code_long_form %notin% fta_validity3$commodity_code_long_form)


fta_2 <- rbind(fta_2, fta_validity3) %>%
  arrange(commodity_code_long_form)

# when removing distinct codes and a code has staging - this is lost. 
# So needs adding back in at end of concatenating process


data2_staging_notes <-
  data %>%
  filter(notes == "X" | notes == "X, A") %>%
  select(commodity_code) %>%
    mutate(notes = "X")
  

fta_2 <- 
  fta_2 %>%
  mutate(notes  = ifelse(commodity_code %in% data2_staging_notes$commodity_code, "X", notes))
  
return(fta_2)


# test assumption: if a country has validity data, the validity data when isolated should not read == "-". 
# if it does read this - error message is returned 
test_validity <- 
  fta_2 %>%
  select(validity) %>%
  distinct(. , validity) %>%
  pull()

if(length(test_validity) == 1){
  
  if(test_validity == "-"){
  
  message("Validity error: validity not concatonated properly")
    
  }
}



} else{
  
  return(data)
  
  # test assumption: if there is no validity - there should be one cell ("-"). If this is not the case - error message is returned
  
  test_no_validity <- 
    data %>% 
    select(validity) %>%
    distinct(. , validity) %>%
    pull()
  
  if(length(test_no_validity) > 1){
    
    message("Validity error: country should not have validity data")
  }
    
    
}

}

# TRQ validity concatenation ---------------------
# description ------------------------------------


trq_validityConcatFunction <- function(data_trq){
  
  #data_trq2 <- trq_data_all_2 %>% filter(trq_country == "Lebanon")
  data_trq2 <- data_trq
  
  trq_validity <- 
    data_trq2 %>% 
    filter(validity_of_duty_rate != "-") %>%
    mutate(quota_code_id = paste0(quota_number, "-", commodity_code)) %>%
    mutate(
      quota_av_duty = round(as.numeric(quota_duty_rate)*100,2),
      quota_duty_rate = 
        ifelse(
          is.na(quota_av_duty),
          quota_duty_rate,
          as.character(paste0(quota_av_duty, "%"))
          )
      ) %>%
    mutate(
      quota_duty_rate_validity = 
        paste0(quota_duty_rate, " ( ", validity_of_duty_rate, " )")
      )
  
  
  # if a country has no validity data - the function just returns what is entered (data2). 
  # Otherwise the data/codes with validity is run through the loop
  if(nrow(trq_validity) >= 1){
    
    trq_code_list <- list()
    
    trq_codes <- unique(trq_validity$quota_code_id)
    
    for(code in trq_codes){
      
      
      trq_validity_filter <- trq_validity %>% filter(quota_code_id == code) %>%
        select(quota_code_id, quota_duty_rate_validity ) %>%
        group_by(quota_code_id) %>%
        mutate(id = 1:n(),
               col_id = paste0("rate","_",id)) %>%
        select( - id) %>%
        pivot_wider(names_from = "col_id", values_from = "quota_duty_rate_validity") %>%
        ungroup()
      
      trq_col_no <- ncol(trq_validity_filter)
      
      
      trq_validity_filter <- 
        trq_validity_filter %>% 
        unite("quota_duty_rate_concat", 2:trq_col_no, sep = " ; " , remove = FALSE) %>%
        select(quota_code_id, quota_duty_rate_concat)
      
      trq_code_list[[code]] <- trq_validity_filter
      
    }
    
    trq_pref_rates_concatonated <- bind_rows(trq_code_list)
    
    trq_validity_2 <- 
      trq_validity %>% 
      left_join(trq_pref_rates_concatonated, by = "quota_code_id") %>%
      distinct(.,quota_code_id , .keep_all = TRUE) %>%
      select(
        fta_country,
        quota_number,
        origin_quota,
        commodity_code,
        commodity_code_cn8,
        quota_duty_rate = quota_duty_rate_concat,
        validity_of_duty_rate,
        quota_volume,
        quota_period,
        units,
        quota_increase,
        notes,
        trq_country,
        quota_code_id
      )
    
    #fta_data <- fta %>% filter(fta_country == "Lebanon")
    
    trq_data_2 <- 
      data_trq2 %>% 
      mutate(quota_code_id = paste0(quota_number, "-", commodity_code)) %>%
      filter(quota_code_id %notin% trq_validity_2$quota_code_id)
    
    trq_data_2 <- 
      rbind(trq_data_2, trq_validity_2) %>%
      arrange(commodity_code) %>%
      select( - quota_code_id)
    
    return(trq_data_2)
  
    
  } else{
    
    return(data_trq2)
    
  }
  
}



# cn10 code consolidation --------------
## description -------------------------





cn10_consolidationFunction <- function(schedule){
  
  # group by cn8 and determine how many rows there are for each cn8 code
  # if a cn8 only appears once - there is no need for any consolidation for any underlying cn10s:
  # adding additional grouping of ukgt and pref rates will create an output with multiple rows for a cn8 if:
  # that cn8 has multiple underlying pref rates/TRQ
  # If there are multiple rows for the cn8 but no different codes, i.e they're all the same - this cn8 will appear once
  
  
  if(country %in% trq_data_all$trq_country) {
    
    data_schedule_final_agg <- schedule %>%  
      mutate(cn8 = substr(final_code, 1, 8)) %>%
      group_by(cn8, preferential_duty_rate_2022, quota_number_concat, trq_code) %>%
      summarise(count_pref = length(preferential_duty_rate_2022), .groups = 'drop')
    
  } else {
    
    data_schedule_final_agg <- schedule %>%  
      mutate(cn8 = substr(final_code, 1, 8)) %>%
      group_by(cn8, preferential_duty_rate_2022) %>%
      summarise(count_pref = length(preferential_duty_rate_2022), .groups = 'drop')
    
  }
  
  # of the aggregated cn8 codes, count how many times a cn8 occurs. If > 1 - this means there are multiple underlying rates
  data_schedule_final_rate_count <- data_schedule_final_agg %>% 
    group_by(cn8) %>%
    summarise(cn8_count = length(cn8), .groups = 'drop')
  
  # match in count of cn8 codes to determine which to consolidate upto cn8
  data_schedule_final4 <- schedule %>% mutate(cn8 = substr(final_code, 1, 8)) %>%
    left_join(data_schedule_final_rate_count, by = "cn8") 
  
  # create unique row id to remove duplicates based upon, where cn8s do not have multiple rates at cn10
  # if TRQ data is presented at a cn10 level - but all rates are the same:
  # these *cannot* be consolidated following this method as the TRQ data is unique to specific cn10s 
  # not necessarily all cn10s under a cn8 code. - trq_data_cn10_flag added so not to consolidate these codes
  
  ## // note cn10 method mentioned above is not included here for HS 2022. Was previously commented out. 
  
  # conditional IF - depending if a dataframe has TRQ data - without this the code breaks:
  if("quota_number_concat" %in% colnames(data_schedule_final4)) {
    
    data_schedule_final4 <- data_schedule_final4 %>%
      mutate(remove_row_id = 
               ifelse(cn8_count > 1, 
                      final_code,
                      paste0(cn8, preferential_duty_rate_2022, cn8_count)))
    
    
    # remove duplicates using row id:
    # for the remaining cn10 codes which have had the other underlying cn10s removed - needs changing to a cn8
    # for cn10s with TRQ data - these are not to change (as shown in the final_code ifelse logic)
    
    data_schedule_final4 <- data_schedule_final4 %>% distinct(., remove_row_id, .keep_all = TRUE) %>%
      mutate(length = str_length(final_code)) %>%
      mutate(final_code = ifelse(length == 10 & cn8_count == 1, #& trq_data_cn10_flag == 0,
                                 cn8, final_code)) %>%
      select(- cn8, - cn8_count, - remove_row_id, - length) #, - trq_data_cn10_flag)
    
    
  } else{
    
    # 02031955 - for mfn quota unique example
    
    data_schedule_final4 <- data_schedule_final4 %>%
      mutate(remove_row_id = ifelse(cn8_count > 1,
                                    final_code,
                                    paste0(cn8, preferential_duty_rate_2022,cn8_count)))
    
    # remove duplicates using row id:
    # for the remaining cn10 codes which have had the other underlying cn10s removed - needs changing to a cn8
    # for cn10s with TRQ data - these are not to change (as shown in the final_code ifelse logic)
    
    data_schedule_final4 <- data_schedule_final4 %>% distinct(., remove_row_id, .keep_all = TRUE) %>%
      mutate(length = str_length(final_code)) %>%
      mutate(final_code = ifelse(length == 10 & cn8_count == 1,
                                 cn8, final_code)) %>%
      select(- cn8, - cn8_count, - remove_row_id, - length)
    
    
  }
  
  
}


# Extrapolation of good codes ------------------------
## description --------------------------------------

extrapFunction <- function(.df, countryName = NULL, .exc = NULL){
  
  
  colNames <- data.frame(colnames(.df))
                         

  
  df_cn2_filter <- 
    .df %>% 
    filter(str_length(commodity_code) == 2 &
            commodity_code_cn8 %notin% eu_codes$cn8)
                         
    mfn_cn2 <- 
      eu_codes %>% 
      filter(cn2 %in% df_cn2_filter$commodity_code) %>% 
      distinct(cn8, .keep_all = TRUE) %>% 
      select(cn8, cn2)
                         
    df_cn2 <- 
      df_cn2_filter %>% 
      left_join(., mfn_cn2, by = c("commodity_code" = "cn2")) %>% 
      rename(final_code = cn8)
                         
                         
      # Filtering/joining all 8 digit codes for cn4 aggregated codes
      # Same methodology as above
                         
     df_cn4_filter <- 
        .df %>% 
        filter(str_length(commodity_code) == 4 &
                commodity_code_cn8 %notin% eu_codes$cn8)
                   
      mfn_cn4 <- 
        eu_codes %>%
          filter(cn4 %in% df_cn4_filter$commodity_code) %>% 
          distinct(cn8, .keep_all = TRUE) %>% 
          select(cn8, cn4)
                         
       df_cn4 <- 
          df_cn4_filter %>% 
          left_join(., mfn_cn4, by = c("commodity_code" = "cn4")) %>% 
          rename(final_code = cn8)
                         
                         
     # Filtering/joining all 8 digit codes for cn6 aggregated codes
     # Same methodology as above
                         
      df_cn6_filter <- 
         .df %>% 
          filter(str_length(commodity_code) == 6 &
          commodity_code_cn8 %notin% eu_codes$cn8)
                         
      mfn_cn6 <- 
        eu_codes %>% 
        filter(cn6 %in% df_cn6_filter$commodity_code) %>% 
        distinct(cn8, .keep_all = TRUE) %>% 
        select(cn8, cn6)
                         
      df_cn6 <- 
        df_cn6_filter %>% 
         left_join(., mfn_cn6, by = c("commodity_code" = "cn6")) %>% 
         rename(final_code = cn8)
                         
                         
     # Filtering cn8 codes that already exist as cn8 codes in EU codes list
                         
      df_cn8_filter <- 
        .df %>% 
        filter(commodity_code_cn8 %in% eu_codes$cn8 &
        str_length(commodity_code) < 10)
                         
       mfn_cn8 <- 
         eu_codes %>% 
         filter(cn8 %in% df_cn8_filter$commodity_code) %>% 
         distinct(cn8, .keep_all = TRUE) %>% 
         select(cn8)
                         
                         
      df_cn8 <- 
        df_cn8_filter %>% 
        left_join(., mfn_cn8, by = c("commodity_code" = "cn8")) %>% 
        mutate(final_code = commodity_code_cn8)
                         
                         
   # Filtering/joining codes that have 10 digit codes
   # Gets all cn10 EU codes for which a preference is listed at the cn10 level 
   # Then checks for all cn10 codes under the cn8 level of the cn10 listed code
   # If there are any additional cn10 codes, tags them as excluded
                         
      df_cn10_filter <- 
        .df %>% 
        filter(str_length(commodity_code) == 10)
                         
      df_cn10 <- 
        df_cn10_filter %>% 
        mutate(final_code = commodity_code)
                         
                         
       mfn_cn10 <- 
         eu_codes %>% 
         select(cn10, cn8) %>% 
         filter(cn8 %in% df_cn10$commodity_code_cn8) 
    
       
    if(!is.null(.exc)){
      
      # name country for print message
      countryName = country
      
      codes_cn10 <- eu_codes %>% 
        select(cn10, cn8) %>% 
        filter(cn8 %in% df_cn10$commodity_code_cn8) 
      
      excluded_codes <- 
        codes_cn10 %>% 
        filter(cn10 %notin% df_cn10$commodity_code_long_form) %>% 
        select(final_code = cn10,  -cn8) %>% 
        mutate(
          fta_country = all_of(country),
          commodity_code_long_form = final_code,
          commodity_code_cn8 = str_sub(final_code, 1, 8),
          commodity_code = final_code,
          preferential_duty_rate_2022 = "excluded",
          validity = "-",
          staging = "-",
          notes = "-"
        ) %>% 
        relocate(final_code, .after = "notes")
      
      df_cn10 <- rbind(df_cn10,excluded_codes)
      
      print(paste0("Appending ", nrow(excluded_codes), " excluded codes for ", countryName))
    }   
                            
                         
  df_full <- rbind(df_cn2, df_cn4, df_cn6, df_cn8, df_cn10)
  
  return(df_full)                       
} 



# Schedule formatting ----------------
## description


scheduleFormatting <- function(schedule){
  
  # upload column names for final output
  col_names_final <- read_excel("..\\inputs\\hs_2022_schedule_colnames.xlsx")
  
  # TRQ colnames ----------------- 
  
  if((trq_countryName %in% trq_data_raw$trq_country)){
    
    data_schedule_final5 <- 
      schedule %>% 
      left_join(product_descriptions, by = "final_code") %>%
      mutate(
        `Reporter name` = "United Kingdom",
        `Agreement name` = agreement_name_for_output,
        pref_av_duty = round(as.numeric(preferential_duty_rate_2022)*100,2),
        `Preferential applied duty rate 2022` = 
          ifelse(
            is.na(pref_av_duty),
            preferential_duty_rate_2022,
            as.character(paste0(pref_av_duty, "%"))
          ),
        `Quota number` = quota_number_concat, 
        `In-Quota Tariff line Code` = trq_code,
        `Preferential applied duty rate excluded` = 
          ifelse(
            `Preferential applied duty rate 2022` == "excluded", "Y","N"),
        `8-digit or 10-digit` = 
          ifelse(
            str_length(final_code) == 10, 10, 8)
      ) %>%
      select(
        - trq_code,
        - quota_number_concat,
        - pref_av_duty,
        - validity,
        - preferential_duty_rate_2022
      ) 
    
    # name column names and find staging columns - to then filter eventual dataframe
    
    col_names <- data.frame(colnames(data_schedule_final5))
    
    staging_col_names <- col_names %>% 
      filter(grepl("staging", col_names$colnames.data_schedule_final5.)) %>%
      filter(colnames.data_schedule_final5. != "staging")
    
    staging_start <- head(staging_col_names,1) %>% pull()
    staging_end   <- tail(staging_col_names,1) %>% pull()
    
    if(nrow(staging_col_names) != 0){
      
      data_schedule_final5 <- 
        data_schedule_final5 %>%
        relocate(. , staging_start:staging_end, .after = `Preferential applied duty rate 2022`) %>%
        relocate(., notes, .after = `Preferential applied duty rate excluded`) %>%
        relocate(., staging, .before = notes) %>%
        relocate(., c(`Reporter name`, `Agreement name`), .before = fta_country) %>%
        relocate(.,  `8-digit or 10-digit`, .after = final_code) 
      
    } else{
      
      data_schedule_final5 <- 
        data_schedule_final5 %>%
        relocate(., notes, .after = `Preferential applied duty rate excluded`) %>%
        relocate(., staging, .before = notes) %>%
        relocate(., c(`Reporter name`, `Agreement name`), .before = fta_country) %>%
        relocate(.,  `8-digit or 10-digit`, .after = final_code) 
      
    }
    
    # changed column names - having filtered for staging data if it exists
    
    col_names_final_filtered <- 
      col_names_final %>% 
      filter(old_col_names %in% col_names$colnames.data_schedule_final5.) %>% 
      pull()
    
    colnames(data_schedule_final5) <- col_names_final_filtered  
    
    
  } else{
    
    # No TRQ colnames -----
    
    data_schedule_final5 <- 
      schedule %>% 
      left_join(product_descriptions, by = "final_code") %>%
      mutate(
        `Reporter name` = "United Kingdom",
        `Agreement name` = agreement_name_for_output,
        pref_av_duty = round(as.numeric(preferential_duty_rate_2022)*100,2),
        `Preferential applied duty rate 2022` = 
          ifelse(
            is.na(pref_av_duty),
            preferential_duty_rate_2022,
            as.character(paste0(pref_av_duty, "%"))
          ),
        `Preferential applied duty rate excluded` = 
          ifelse(
            `Preferential applied duty rate 2022` == "excluded", "Y","N"),
        `8-digit or 10-digit` = 
          ifelse(str_length(final_code) == 10, 10, 8)
      ) %>%
      select(
        - pref_av_duty,
        - validity,
        - preferential_duty_rate_2022
      ) 
    
    # name column names and find staging columns - to then filter eventual dataframe
    
    col_names <- data.frame(colnames(data_schedule_final5))
    
    staging_col_names <- col_names %>% filter(grepl("staging", col_names$colnames.data_schedule_final5.))
    
    staging_start <- head(staging_col_names,1) %>% pull()
    staging_end   <- tail(staging_col_names,1) %>% pull()
    
    if(nrow(staging_col_names) != 0){
      
      data_schedule_final5 <- 
        data_schedule_final5 %>%
        relocate(. , staging_start:staging_end, .after = `Preferential applied duty rate 2022`) %>%
        relocate(., notes, .after = `Preferential applied duty rate excluded`) %>%
        relocate(., staging, .before = notes) %>%
        relocate(., c(`Reporter name`, `Agreement name`), .before = fta_country) %>%
        relocate(.,  `8-digit or 10-digit`, .after = final_code) 
      
    } else{
      
      
      data_schedule_final5 <- 
        data_schedule_final5 %>%
        relocate(., notes, .after = `Preferential applied duty rate excluded`) %>%
        relocate(., staging, .before = notes) %>%
        relocate(., c(`Reporter name`, `Agreement name`), .before = fta_country) %>%
        relocate(.,  `8-digit or 10-digit`, .after = final_code) 
      
    }
    
    # changed column names - having filtered for staging data if it exists
    
    col_names_final_filtered <- 
      col_names_final %>% 
      filter(old_col_names %in% col_names$colnames.data_schedule_final5.) %>% 
      pull()
    
    colnames(data_schedule_final5) <- col_names_final_filtered  
  }
  
  
  # # QA check: --
  # 
  # product_code_qa <- data_schedule_final5 %>% filter(is.na(`Commodity code description`)) 
  # 
  # if(!(nrow(product_code_qa) == 0)){
  #   
  #   message(paste0("Not all commodity codes have self-contained descriptions for ", country))
  # }
  
  
  data_schedule_final5[is.na( data_schedule_final5)] <- "n/a"
  return(data_schedule_final5)
  
}


# Excel workbook function ------------------------------------
## description -----------------------------------------------
#'
#'


excel_wbFunction <- function(df_goods, df_trq_data, country){
  
  
  #' file name output folder number is aligned with the full fta_notificaiotn code version
  #' 11/03 - verison 5.
  #' 24/03 - version 6.
  
  file_name <- paste0("..\\outputs\\fta_outputs\\outputs6\\"
                      ,country,"_hs2022_schedule.xlsx")
  
  workbook <- createWorkbook()
  
  # create worksheets:
  
  addWorksheet(workbook, sheet = "goods_schedule", zoom = 80)
  
  if(!is.null(df_trq_data)){addWorksheet(workbook, sheet = "trq_data", zoom = 90)}
  
  
  if(!is.null(df_trq_data)){
    
    writeData(workbook, "goods_schedule", df_goods)
    writeData(workbook, "trq_data", df_trq_data)
    
  } else{
    print(paste0(country," :No TRQ data in excel output"))
    
    writeData(workbook, "goods_schedule", df_goods)
  }
  
  
  # workbook styles ------------------------------------------------------
  
  workbook_colNames <- data.frame(colnames(df_goods))
  workbook_colNames <-
    workbook_colNames %>%
    mutate(order = 1:n()) 
  
  colnames(workbook_colNames) <- c("Colnames","order")
  
  # staging start and end columns - used to automate the staging output columns - that the 
  # excel formatting works regardless of how many staigng columns are in outputs
  
  staging_order <- 
    workbook_colNames %>%
    filter(grepl("Staging", workbook_colNames$Colnames)) %>%
    filter(Colnames != "Staging") # filter non-preferential staging column
  
  staging_col_start <- head(staging_order,1) %>% select(order) %>% pull()
  staging_col_end   <- tail(staging_order,1) %>% select(order) %>% pull()
  
  staging_ends <- (staging_col_end +1)
  df_end <- length(df_goods)
  
  # logic to automate excel outputs with TRQ data tabs. 
  if(!is.null(df_trq_data)){
    # staging logic - extend out formatting to staging columns  
    ###  TRQ & staging ----------------------------------------------------------------
    if(nrow(staging_order) > 0){  
      
      # goods schedule tab:
      
      headerStyle <- 
        createStyle(
          fontSize = 11, 
          halign = "center",
          textDecoration = "bold"
        )
      
      
      addStyle(
        workbook, 
        "goods_schedule",
        headerStyle, 
        rows = 1, 
        cols = 1:df_end, 
        gridExpand = TRUE
      )
      
      # set column widths
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(1:3,5, 6, staging_ends:df_end), 
        widths =  15
      )
      
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(4,7,8,9:11, staging_col_start:staging_col_end), 
        widths = 30
      )
      
    } else{
      
      
      headerStyle <- 
        createStyle(
          fontSize = 11, 
          halign = "center",
          textDecoration = "bold"
        )
      
      addStyle(
        workbook, 
        "goods_schedule",
        headerStyle, 
        rows = 1, 
        cols = 1:14, 
        gridExpand = TRUE
      )
      
      # set column widths:
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(1:3,5, 6,12:13), 
        widths =  15
      )
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(2,9:11), 
        widths =  25
      )
      
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(4,7,8), 
        widths = 30
      )
      
      
      
      freezePane(workbook, "goods_schedule", firstActiveRow = 2, firstActiveCol = 1)
      
    } # end staging conditional logic
    
    #### TRQ sheet --------------------------------------------------------------
    
    
    # TRQ data:
    headerStyle <- 
      createStyle(
        fontSize = 11, 
        halign = "center", 
        textDecoration = "bold"
      )
    
    addStyle(
      workbook, 
      "trq_data",
      headerStyle, 
      rows = 1, 
      cols = 1:13, 
      gridExpand = TRUE
    )
    
    
    freezePane(workbook, "trq_data", firstActiveRow = 1, firstActiveCol = 1)
    
    # set column widths
    setColWidths(
      workbook, 
      "trq_data", 
      cols = c(1:13), 
      widths =  25
    ) 
    
    
  } else{
    ### Non-TRQ staging ----------------------------
    # formatting excel output if no TRQ data:
    
    # goods schedule tab:
    
    if(nrow(staging_order) > 0){  
      
      
      # goods schedule tab:
      
      headerStyle <- 
        createStyle(
          fontSize = 11, 
          halign = "center",
          textDecoration = "bold"
        )
      
      
      addStyle(
        workbook, 
        "goods_schedule",
        headerStyle, 
        rows = 1, 
        cols = 1:df_end, 
        gridExpand = TRUE
      )
      
      
      # set column widths
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(1:3,5, 6, staging_ends:df_end), 
        widths =  15
      )
      
      
      
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(4,7,8,9:11,staging_col_start:staging_col_end), 
        widths = 30
      )
      
    } else{
      
      ### Non-TRQ non-staging -------------------
      
      headerStyle <- 
        createStyle(
          fontSize = 11, 
          halign = "center",
          textDecoration = "bold"
        )
      
      
      addStyle(
        workbook, 
        "goods_schedule",
        headerStyle, 
        rows = 1,
        cols = 1:14, 
        gridExpand = TRUE
      )
      
      # set column widths
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(1,3,5, 6,9:11), 
        widths =  15
      )
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(2), 
        widths =  25
      )
      
      
      setColWidths(
        workbook, 
        "goods_schedule", 
        cols = c(4,7,8), 
        widths = 30
      )
      
    } # end staging conditional logic
    
    
  } # end Non-TRQ logic
  
  
  
  # Save final workbook ------------------------------------
  
  saveWorkbook(workbook, file = file_name, overwrite = TRUE)  
  
  print(paste0("Excel workbook created for ", country))
  
}