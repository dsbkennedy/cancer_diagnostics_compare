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

raw_15_16_new <- old_format_fn('data/did_15_16.xlsx') 
names(raw_15_16_new) <- all_months
raw_15_16_new_append <- data.table::rbindlist(raw_15_16_new, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ 2015, 
                                                                                                      TRUE ~ 2016))
raw_16_17_new <- old_format_fn('data/did_16_17.xlsx') 
names(raw_16_17_new) <- all_months
raw_16_17_new_append <- data.table::rbindlist(raw_16_17_new, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ 2016, 
                                                                                                      TRUE ~ 2017))
raw_17_18_new <- old_format_fn('data/did_17_18.xlsx') 
names(raw_17_18_new) <- all_months
raw_17_18_new_append <- data.table::rbindlist(raw_17_18_new, idcol="month") %>% mutate(year=case_when(month %in% first9_months ~ 2017, 
                                                                                                      TRUE ~ 2018))

raw_15_16_17_18 <- bind_rows(raw_15_16_new_append, raw_16_17_new_append, raw_17_18_new_append)

rm(raw_15_16_new, raw_15_16_new_append, raw_16_17_new, raw_16_17_new_append,raw_17_18_new, raw_17_18_new_append)

#### Newer format
download.file(url_18_19, 'did_18_19.xlsx')

(raw_18_19 <- readxl::read_xlsx('data/did_18_19.xlsx', sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-year) %>% 
    pivot_longer(-c(1:4)) %>% rename(month=name) %>% 
    mutate(year=case_when(month %in% first9_months ~ 2018,
                                                    TRUE ~ 2019))
)

(raw_19_20 <- readxl::read_xlsx('data/did_19_20.xlsx', sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-year) %>% 
    pivot_longer(-c(1:4)) %>% rename(month=name) %>% 
    mutate(year=case_when(month %in% first9_months ~ 2019,
                          TRUE ~ 2020))
)

(raw_20_21 <- readxl::read_xlsx('data/did_20_21.xlsx', sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-year) %>% 
    pivot_longer(-c(1:4)) %>% rename(month=name) %>% 
    mutate(year=case_when(month %in% first9_months ~ 2020,
                          TRUE ~ 2021))
)

(raw_21_22 <- readxl::read_xlsx('data/did_21_22.xlsx', sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-year_to_date) %>% 
    pivot_longer(-c(1:4)) %>% rename(month=name) %>% 
    mutate(year=case_when(month %in% first9_months ~ 2021,
                          TRUE ~ 2022))
)

all_data <- mget(ls(pattern="^raw")) %>%
  bind_rows()




download_fn <- function(x,y) {
  download.file(x, y)
}
download_fn(did_15_16, 'did_15_16.xlsx')

list_files <- c('did_15_16', 'did_16_17')

map(download.file, list_files)

download_all_files_fn <- function(x) {
  url <- x
  destfile <- paste0(x, '.xlsx')
  return(url)
}

download_all_files_fn(did_15_16)

  
download.file(did_15_16, 'did_15_16.xlsx')

(raw_15_16 <- readxl::read_xlsx('did_15_16.xlsx', sheet='Table 1a', skip=13) %>% 
  clean_names() %>% 
  filter(provider_name!='England' & !is.na(provider_name)) %>% 
    mutate(across(-c('region_code', 'org_code', 'provider_name'), as.character)) %>% 
    pivot_longer(-c('region_code', 'org_code', 'provider_name'))
)


library(readxl)

spreadsheet_fn <- function(x) {
  clean_names(x) %>% 
    filter(provider_name!='England' & !is.na(provider_name)) %>% 
    mutate(across(-c('region_code', 'org_code', 'provider_name'), as.character)) %>% 
    pivot_longer(-c('region_code', 'org_code', 'provider_name'))
}

path <- 'did_15_16.xlsx'

raw_15_16 <- path %>%
  excel_sheets()  %>% 
  purrr::discard(df,.p = ~stringr::str_detect(.x,"Title Page")) %>% 
  set_names() %>%
  map(read_xlsx,
      path = path, skip=13) %>% 
  map(spreadsheet_fn)

names(raw_15_16) <- months
  
months <- c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar')  


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

raw_15_16_new <- old_format_fn('did_15_16.xlsx') 
names(raw_15_16_new) <- months
raw_15_16_new_append <- data.table::rbindlist(raw_15_16_new, idcol="month") %>% mutate(year=case_when(month %in% c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec') ~ 2015, 
                                                                                                      TRUE ~ 2016))
raw_16_17_new <- old_format_fn('did_16_17.xlsx') 
names(raw_16_17_new) <- months
raw_16_17_new_append <- data.table::rbindlist(raw_16_17_new, idcol="month") %>% mutate(year=case_when(month %in% c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec') ~ 2016, 
                                                                                                      TRUE ~ 2017))
raw_17_18_new <- old_format_fn('did_17_18.xlsx') 
names(raw_17_18_new) <- months
raw_17_18_new_append <- data.table::rbindlist(raw_17_18_new, idcol="month") %>% mutate(year=case_when(month %in% c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec') ~ 2017, 
                                                                                                      TRUE ~ 2018))

raw_15_16_17_18 <- bind_rows(raw_15_16_new, raw_16_17_new, raw_17_18_new)

rm(raw_15_16_new, raw_15_16_new_append, raw_16_17_new, raw_16_17_new_append,raw_17_18_new, raw_17_18_new_append)















download.file(url_16_17, 'did_16_17.xlsx')

(raw_16_17 <- readxl::read_xlsx('did_16_17.xlsx', sheet='Table 1a', skip=13) %>% 
    clean_names() %>% 
    filter(provider_name!='England' & !is.na(provider_name)) %>% 
    mutate(across(-c('region_code', 'org_code', 'provider_name'), as.character)) %>% 
    pivot_longer(-c('region_code', 'org_code', 'provider_name'))
)


download.file(url_17_18, 'did_17_18.xlsx')

(raw_17_18 <- readxl::read_xlsx('did_17_18.xlsx', sheet='Table 1a', skip=13) %>% 
    clean_names() %>% 
    filter(provider_name!='England' & !is.na(provider_name)) %>% 
    mutate(across(-c('region_code', 'org_code', 'provider_name'), as.character)) %>% 
    pivot_longer(-c('region_code', 'org_code', 'provider_name'))
)

download.file(url_18_19, 'did_18_19.xlsx')

(raw_18_19 <- readxl::read_xlsx('did_18_19.xlsx', sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-year)
)

download.file(url_19_20, 'did_19_20.xlsx')

(raw_19_20 <- readxl::read_xlsx('did_19_20.xlsx', sheet='Provider', skip=12) %>% 
    clean_names() %>% 
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% 
    select(-year)
)




download.file(url_21_22, 'did_21_22.xlsx')
raw_21_22 <- readxl::read_xlsx('did_21_22.xlsx', sheet='Provider', skip=11) %>% 
  clean_names() %>% 
  filter(provider_name!='ENGLAND')














import_old_format_fn <- function(year) {
  #download.file(x, paste0(x,'.xlsx'))
  #download.file(x, 'url_15_16.xlsx')
  filename <- paste0(year, 'xlsx')
  #download.file(x, paste0(enquote(x),'.xlsx'))
  return(filename)
  
}

import_old_format_fn(url_15_16)

download.file(url_15_16, 'url_15_16.xlsx')





