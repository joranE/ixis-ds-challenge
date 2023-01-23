library(tidyverse)
library(janitor)
library(skimr)
library(lubridate)
library(writexl)

theme_set(theme_bw())

# Read data
session_counts <- readr::read_csv(file = 'data/DataAnalyst_Ecom_data_sessionCounts.csv',
                                  show_col_types = FALSE) %>%
  janitor::clean_names()
adds_to_cart <- readr::read_csv(file = 'data/DataAnalyst_Ecom_data_addsToCart.csv',
                                show_col_types = FALSE) %>%
  janitor::clean_names()

# Quick summary to scan for missingness, unusual values
skimr::skim(session_counts)
skimr::skim(adds_to_cart)

# Ensure dim_date is a date
session_counts <- session_counts %>%
  mutate(dim_date = as.Date(dim_date,'%m/%d/%y'))

# Create a true monthly date column to ease filtering & any potential joins
adds_to_cart <- adds_to_cart %>%
  mutate(month = as.Date(paste(dim_year,
                               stringr::str_pad(dim_month,width = 2,side = 'left',pad = '0'),
                               '01',sep = '-'))) %>%
  relocate(month,.before = everything())

# Month * Device aggregation with the metrics:
# - Session
# - Transactions
# - QTY
# - ECR (Transactions / Sessions)
month_device_metrics <- session_counts %>%
  mutate(month = lubridate::floor_date(dim_date,'month')) %>%
  group_by(month,dim_device_category) %>%
  summarise(across(where(is.numeric),sum),.groups = 'drop') %>%
  mutate(ecr = transactions / sessions)

# Month-over-month metrics (by device category as above) for May/June 2013
# including absolute & relative differences
mom_metrics <- month_device_metrics %>%
  filter(month >= as.Date('2013-05-01')) %>%
  mutate(month = lubridate::month(month,abbr = TRUE,label = TRUE)) %>%
  pivot_longer(cols = where(is.numeric),names_to = 'metric',values_to = 'val') %>%
  pivot_wider(names_from = 'month',values_from = val) %>%
  mutate(abs_diff = Jun - May,
         rel_diff = (Jun - May) / May)

# Create a corresponding row for adds-to-cart to append to the rest of the 
# month-over-month metrics
mom_adds <- adds_to_cart %>%
  filter(month >= as.Date('2013-05-01')) %>%
  mutate(dim_device_category = 'all',
         metric = 'adds_to_cart',
         month = lubridate::month(month,label = TRUE,abbr = TRUE)) %>%
  select(dim_device_category,metric,month,adds_to_cart) %>%
  pivot_wider(names_from = month,values_from = adds_to_cart) %>%
  mutate(abs_diff = Jun - May,
         rel_diff = (Jun - May) / May)

mom_metrics <- bind_rows(mom_metrics,mom_adds)

# Write to Excel
writexl::write_xlsx(x = list(month_device_metrics = month_device_metrics,
                             month_over_month = mom_metrics),
                    path = 'output/metrics_ref_tables.xlsx')