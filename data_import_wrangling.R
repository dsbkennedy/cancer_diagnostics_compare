# Load packages -----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse,janitor,readxl, lubridate, skimr,zoo)
# Load  data --------------------------------------------------------------
url_stem <- 'https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/'
# PRE-COVID
url_18_19 <- paste0(url_stem, '2019/12/DID-Table-1-2018-19-Modality-Provider-Counts-xlsx-184KB.xlsx')
url_19_20 <- paste0(url_stem,'2020/10/DID-Table-1-2019-20-Modality-Provider-Counts-xlsx-187KB.xlsx')
# COVID
url_20_21 <- paste0(url_stem,'2021/11/DID-Table-1-2020-21-Modality-Provider-Counts-xlsx-181KB.xlsx')
url_21_22 <- paste0(url_stem,'2022/08/DID-Table-1-2021-22-Modality-Provider-Counts-2022-08-18-XLSX-197KB.xlsx')
rm(url_stem)
dir.create('data') # Create directory for data
list_of_urls <- mget( ls( pattern = "url_" ) ) #Combine URLs into one list
file_names <- paste0('data/did_', c(18:21), '_', c(19:22), '.xlsx') # Make names for files
safe_download <- safely(~ download.file(.x , .y, mode = "wb")) # Download files
walk2(list_of_urls, file_names, safe_download)
rm(list = ls()) # Clean up workspace
all_months <- c('apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar')  # List months in order they appear in data
first9_months <- all_months[1:9] # First 9 months are used to assign calendar year
#### Format excel files
format_fn <- function(x,y) {
  filepath <- paste0('data/did_',x,'_',y,'.xlsx') # Generate file path
  raw <- read_xlsx(filepath, sheet='Provider', skip=12) %>% # Skip extraneous rows
    clean_names() %>%  # Clean column names
    filter(provider_name!='ENGLAND' & !is.na(provider_name)) %>% # Remove summary data
    select(-contains('year')) %>% # Remove year variable
    pivot_longer(-c(1:4)) %>% # Reshape data
    rename(month=name) %>% # Rename variable to month
    mutate(year=case_when(month %in% first9_months ~ x, TRUE ~ y),
           month_year=as.yearmon(paste0(month,'-', year), "%b-%y"),
           month_factor=month(month_year, label=TRUE),
           count=as.numeric(value)) %>% select(-c(month, value))
}
analysis_data <- map2(c(18:21), c(19:22), format_fn) %>% bind_rows # Run code for 4 years of data
skim(analysis_data) # Overview of data completeness
# Counts <5 are reported as "*" to avoid identifying individuals and are therefore "missing" in the analysis_data.

# Graph to check data
analysis_data %>%  filter(modality =='Plain Radiography') %>% 
  mutate(month_year=as.yearmon(paste0(month,'-', year), "%b-%y")) %>% 
  mutate(month_factor=month(month_year, label=TRUE)) %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(year, month_factor, modality) %>% 
  summarise(total=sum(value, na.rm=TRUE)) %>% 
  ungroup() %>% 
  ggplot(aes(x=month_factor, y=total, group=year)) +
  geom_line(aes(color=as.factor(year))) +
  gghighlight::gghighlight(year>=20) +
  scale_y_continuous(limits=c(0,NA), labels = unit_format(unit = "M", scale = 1e-6)) +
  labs(x='', y='', title = 'Count of plain radiography events from 2018-2022 from the Diagnostic Imaging Dataset (NHS England)', 
       subtitle = '2020-2022 are highlighted to demonstrate the impact of COVID-19 on diagnostic services') +
  theme_minimal() +
  theme(text=element_text(size=20))



