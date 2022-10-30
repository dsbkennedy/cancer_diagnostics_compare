# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse,janitor,readxl,scales,gghighlight)
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

dir.create('data') # Create directory for data
list_of_urls <- mget( ls( pattern = "url_" ) ) #Combine URLs into one list
file_names <- paste0('data/did_', c(15:22), '_', c(16:23), '.xlsx') # Make names for files
safe_download <- safely(~ download.file(.x , .y, mode = "wb")) # Download files
walk2(list_of_urls, file_names, safe_download)
rm(list = ls()) # Clean up workspace

# Older files have different format (1 tab for each month)
spreadsheet_fn <- function(x) {
  clean_names(x) %>% 
    filter(provider_name!='England' & !is.na(provider_name)) %>% # Filter out summary data
    mutate(across(-c('region_code', 'org_code', 'provider_name'), as.character)) %>%  # Convert columns to character
    pivot_longer(-c('region_code', 'org_code', 'provider_name')) # Reshape data
}

old_format_fn <- function(x) {
  path <- x
  raw <- path %>%
    excel_sheets()  %>% # Get names of excel sheets
    purrr::discard(df,.p = ~stringr::str_detect(.x,"Title Page")) %>% # Remove sheet called "Title Page"
    set_names() %>% # Name data frames based on sheet name
    map(read_xlsx,
        path = path, skip=13) %>% # Skip extraneous rows
    map(spreadsheet_fn) # Apply spreadsheet_fn
  return(raw)
}

all_months <- c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar')  # List months in order they appear in data
first9_months <- all_months[1:9] # First 9 months are used to assign calendar year

old_additional_format_fn <- function(x,y) {
  filepath <- paste0('data/did_',x,'_',y,'.xlsx') # Generate filepath
  raw <- old_format_fn(filepath)  # Apply old_format_fn
  names(raw) <- all_months # Rename data frames based on month variable
  raw_append <- data.table::rbindlist(raw, idcol="month") %>% # Append data
    mutate(year=case_when(month %in% first9_months ~ x, TRUE ~ y)) # Label year based on first 9 months
}
raw_15_16_17 <- map2(c(15:17), c(16:18), old_additional_format_fn) %>% bind_rows # Run code for first 3 years of data

#### Newer format
new_format_fn <- function(x,y) {
  filepath <- paste0('data/did_',x,'_',y,'.xlsx') # Generate filepath
  raw <- readxl::read_xlsx(filepath, sheet='Provider', skip=12) %>% # Skip extraneous rows
    clean_names() %>%  # Clean column names
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% # Remove summary data
    select(-contains('year')) %>% # Remove year variable
    pivot_longer(-c(1:4)) %>% # Reshape data
    rename(month=name) %>% # Rename variable to month
    mutate(year=case_when(month %in% first9_months ~ x, TRUE ~ y)) # Label year based on first 9 months
}

raw_18_19_20_21_22 <- map2(c(18:21), c(19:22), new_format_fn) %>% bind_rows # Run code for additional 5 years of data

all_data <- bind_rows(raw_15_16_17,raw_18_19_20_21_22) %>% # Combine all data
  mutate(modality_complete=case_when(is.na(modality) ~ name,  TRUE ~ modality)) %>% # Name of modality variable changes from old to new format
  mutate(modality_complete=tolower(str_replace_all(modality_complete, ' ', '_'))) # Format of data in modality variable also changes so needs to be harmonised

rm(list=setdiff(ls(), c("all_data"))) # Clean up workspace


# Graph to check data
all_data %>%   filter(modality_complete =='plain_radiography') %>% 
  mutate(month_year=as.yearmon(paste0(month,'-', year), "%b-%y")) %>% 
  mutate(month_factor=lubridate::month(month_year, label=TRUE)) %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(year, month_factor, modality_complete) %>% 
  summarise(total=sum(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x=month_factor, y=total, group=year)) +
  geom_line(aes(color=as.factor(year))) +
  gghighlight::gghighlight(year>=20) +
  scale_y_continuous(limits=c(0,NA), labels = unit_format(unit = "M", scale = 1e-6)) +

  labs(x='', y='', title = 'Count of plain radiography events from 2015-2022 from the Diagnostic Imaging Dataset (NHS England)', 
       subtitle = '2020-2022 are highlighted to demonstrate the impact of COVID-19 on diagnostic services') +
  theme_minimal() +
  theme(text=element_text(size=20))



