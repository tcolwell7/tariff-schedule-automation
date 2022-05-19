# HS 2022 tariff schedules data cleaning

#' v1 - 15/02/22 - Code copied from WTO notifications work and initial start to update to reflect hs 2022 data. 
#'    - 16/02/22 - UKGT lines removed. Product dsc. added. Feb EU codes added. 
#'    - 17/02/22 - Update to include EU nomenclature full for code descriptions. Previous self-contained descriptions are only for 2021 codes and prior. Need dscs, for 2022 codes. 
#' v2 - 04/03/22 - Removal of format tariffs function and moved to scheduleFunctions script. This is to reduce the instances this function is repeated across scripts. 
#'    - 07/03/22 - New country input file + QA check added to fix aggregated commodity codes whcih are still in long format. 
#'    - 11/03/22 - pref data 2 now refered to following the previous data update. (More data updates are expected soon). 
#'    - 24/03/22 - updated pref data to v3
#' v3 - 25/03/22 - Pref data updated removing columns - code updated to reflect removal of columns. 
#'

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("scheduleFunctions.R")
# Read data ---------------------------------------------------------------

#notes <- read_excel("..\\inputs\\notes_page.xlsx")

# data for schedule - 

fta_raw <- read.xlsx("..\\inputs\\pref_data3.xlsx") %>% clean_names()
eu_codes_raw <- read.xlsx("..\\inputs\\eu_codes_feb_2022.xlsx", sheet = "declarable_codes") %>% clean_names()
eu_codes_full <- read.xlsx("..\\inputs\\eu_codes_feb_2022.xlsx", sheet = "full_nomenclature") %>% clean_names()
country_input <- read.xlsx("..\\inputs\\hs_2022_country_input.xlsx") %>% clean_names



# EU codes dsc --------------

eu_codes_dsc <- eu_codes_full %>% 
  clean_names() %>%
  mutate(cn10 = str_sub(goods_code,1,10)) %>%
  group_by(cn10) %>%
  summarise(
    count = length(cn10),
    name = paste0(description, collapse = " - ")
    ) %>%
  mutate(cn8 = str_sub(cn10,1,8))
  
eu_codes_dsc_cn8 <- eu_codes_dsc %>% distinct(., cn8, .keep_all = TRUE)

# product code descriptions ----

product_descriptions <-
  read_excel("..\\inputs\\product_descriptions_goods.xlsx") %>% clean_names() %>%
  mutate(final_code = ifelse(hier_pos != "10", substr(commodity_code_10_digit, 1, 8),
                             commodity_code_10_digit)) %>%
  select(-hier_pos, - commodity_code_10_digit)



# commodity headings to maTtch into schedules and import outputs:
commodity_headings <- 
  read_excel("..\\inputs\\commodity_headings.xlsx") %>% 
  clean_names() %>%
  mutate(hs2 = str_sub(commodity_heading,1,2))

# EU codes ----


eu_codes <- eu_codes_raw %>% 
  clean_names() %>% 
  select(cn10 = commodity_code) %>% 
  mutate(cn10 = substr(cn10, 1, 10),
         cn8 = substr(cn10, 1, 8),
         cn6 = substr(cn10, 1, 6),
         cn4 = substr(cn10, 1, 4),
         cn2 = substr(cn10, 1, 2)) %>% 
  filter(cn2 %notin% c("98", "99"))


# Tidying FTA country data ----
# Removing dots  in commodity_code columns and creating cn8 column

fta <- fta_raw %>% 
  clean_names() %>% 
  mutate(
    across(
      c(commodity_code,commodity_code_long_form), ~ str_remove_all(.x , "x"))) %>%
  mutate(
    across(
      c(commodity_code,commodity_code_long_form), ~ gsub("[.]", "", .x)),
    commodity_code_cn8 = str_sub(commodity_code_long_form,1,8)
    ) %>% ## can't use str_remove (.) as the across function reads the . as the column. 
  relocate(commodity_code_cn8, .after = "commodity_code") %>%
  relocate(commodity_code_long_form, .before = "commodity_code")



fta <- format_tariff_rates(fta, "preferential_duty_rate_2022")


# FTA long format check:


fta <-
  fta %>% 
  mutate(str_r = str_sub(commodity_code, 9,10)) %>%
  mutate(
    commodity_code =
      ifelse(
        str_r == "00",
        str_sub(commodity_code,1,8),
        commodity_code
      )
  ) %>%
  select(-str_r, -length)




