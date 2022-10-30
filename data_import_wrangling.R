# Load packages -----------------------------------------------------------
library(tidyverse)
library(janitor)
library(readxl)

# Load  data --------------------------------------------------------------
url_stem <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/'
# PRE-COVID
url_15_16 <- paste0(url_stem, '2015/08/Tables-1a-1l-2015-16-Modality-Provider-Counts-XLSX-284KB.xlsx')
url_16_17 <- paste0(url_stem, '2017/11/Tables-1a-1l-2016-17-Modality-Provider-Counts-XLSX-405KB.xlsx')
url_17_18 <- paste0(url_stem, '2018/11/Tables-1a-1l-2017-18-Modality-Provider-Counts-XLSX-222KB.xlsx')
url_18_19 <- paste0(url_stem, '2019/12/DID-Table-1-2018-19-Modality-Provider-Counts-xlsx-184KB.xlsx')
url_19_20 <- paste0(url_stem,'2020/10/DID-Table-1-2019-20-Modality-Provider-Counts-xlsx-187KB.xlsx')
# COVID
url_20_21 <- paste0(url_stem,'2021/11/DID-Table-1-2020-21-Modality-Provider-Counts-xlsx-181KB.xlsx')
url_21_22 <- paste0(url_stem,'2022/08/DID-Table-1-2021-22-Modality-Provider-Counts-2022-08-18-XLSX-197KB.xlsx')
url_22_23 <- paste0(url_stem,'2022/10/DID-Table-1-2022-23-Modality-Provider-Counts-2022-10-27-XLSX-180KB.xlsx')

rm(url_stem)
# Create directory for data
dir.create('data')
#Combine urls into one list
list_of_urls <- mget( ls( pattern = "url_" ) )
# Make names for files
file_names <- paste0('data/did_', c(15:22), '_', c(16:23), '.xlsx')
# Download files
safe_download <- safely(~ download.file(.x , .y, mode = "wb"))
walk2(list_of_urls, file_names, safe_download)
# Clean up workspace
rm(list = ls(pattern = "^url_"))

# Older files have different format (1 tab for each month)

spreadsheet_fn <- function(x) {
  clean_names(x) %>% 
    filter(provider_name!='England' & !is.na(provider_name)) %>% 
    mutate(across(-c('region_code', 'org_code', 'provider_name'), as.character)) %>% 
    pivot_longer(-c('region_code', 'org_code', 'provider_name'))
}

old_format_fn <- function(x) {
  path <- x
  raw <- path %>%
    excel_sheets()  %>% 
    purrr::discard(df,.p = ~stringr::str_detect(.x,"Title Page")) %>% 
    set_names() %>%
    map(read_xlsx,
        path = path, skip=13) %>% 
    map(spreadsheet_fn) 
  return(raw)
}

all_months <- c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar')  
first9_months <- all_months[1:9]

old_additional_format_fn <- function(x,y) {
  
  filepath <- paste0('data/did_',x,'_',y,'.xlsx')
  raw <- old_format_fn(filepath) 
  names(raw) <- all_months
  raw_append <- data.table::rbindlist(raw, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ x, 
                                                                                                        TRUE ~ y))
}
raw_15_16_17 <- map2(c(15:17), c(16:18), old_additional_format_fn) %>% bind_rows

# raw_15_16_new <- old_format_fn('data/did_15_16.xlsx') 
# names(raw_15_16_new) <- all_months
# raw_15_16_new_append <- data.table::rbindlist(raw_15_16_new, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ 2015, 
#                                                                                                       TRUE ~ 2016))
# raw_16_17_new <- old_format_fn('data/did_16_17.xlsx') 
# names(raw_16_17_new) <- all_months
# raw_16_17_new_append <- data.table::rbindlist(raw_16_17_new, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ 2016, 
#                                                                                                       TRUE ~ 2017))
# raw_17_18_new <- old_format_fn('data/did_17_18.xlsx') 
# names(raw_17_18_new) <- all_months
# raw_17_18_new_append <- data.table::rbindlist(raw_17_18_new, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ 2017, 
#                                                                                                       TRUE ~ 2018))
# raw_15_16_17_18 <- bind_rows(raw_15_16_new_append, raw_16_17_new_append, raw_17_18_new_append)
# 
# rm(raw_15_16_new, raw_15_16_new_append, raw_16_17_new, raw_16_17_new_append,raw_17_18_new, raw_17_18_new_append)

#### Newer format
new_format_fn <- function(x,y) {
  
  filepath <- paste0('data/did_',x,'_',y,'.xlsx')
  
  raw <- readxl::read_xlsx(filepath, sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-contains('year')) %>% 
    pivot_longer(-c(1:4)) %>% rename(month=name) %>% 
    mutate(year=case_when(month %in% first9_months ~ x,
                          TRUE ~ y))
  
}

raw_18_19_20_21_22 <- map2(c(18:21), c(19:22), new_format_fn) %>% bind_rows

all_data <- bind_rows(raw_15_16_17_18,raw_18_19_20_21_22)



