
library(janitor)
library(skimr)
library(readxl)
library(visdat)
library(naniar)
library(skimr)
library(lubridate)
library(googlesheets)
library(tidyverse)

# Data Import and Cleaning Setup

wk <- read.csv("~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/wk2_data.csv", check.names = F, stringsAsFactors = F, na.strings = "")


asdf <- read_csv("~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/wk2_data.csv")



air1 <- airquality %>% 
  as_tibble() %>% 
  add_column(Year = 2013) %>% 
  bind_rows(airquality %>% 
              mutate_all(funs(sample(., length(.)))) %>% 
              add_column(Year = 2014)) %>% 
  mutate(Day = ifelse(Day == 31, 30, Day)) %>% 
  mutate(date = mdy(paste(Month, Day, Year))) %>% 
  arrange(date) %>% 
  distinct(date, .keep_all = T) %>% 
  as_tibble()


air2full <- airquality %>% 
  mutate_all(funs(sample(., length(.)))) %>% 
  add_column(Year = 2015) %>% 
  mutate(Day = ifelse(Day == 31, 30, Day)) %>% 
  mutate(date = mdy(paste(Month, Day, Year))) %>% 
  distinct(date, .keep_all = T) %>% 
  bind_rows(air1 %>% filter(Year == 2014)) %>% 
  arrange(date) %>% 
  as_tibble()

air2 <- air2full %>% 
  mutate(Temp = NA)

air2temp <- air2full %>% 
  mutate(Temp = round((Temp - 32) / 1.8, digits = 2)) %>% 
  select(-Ozone, -Solar.R, -Wind)


air1 <- air1 %>% 
  rename("Date?" = date,
         "Ozone layer" = Ozone,
         "Wind speed (miles/hour)" = Wind,
         "Temp. in F" = Temp) %>% 
  select(-Month, -Day, -Year)

air2 <- air2 %>% 
  rename("Enter date:" = date,
         "Ozone layer" = Ozone,
         "Wind speed (miles/hour)" = Wind) %>% 
  select(-Month, -Day, -Year)

air2temp <- air2temp %>% 
  rename("Date?" = date) %>% 
  select(-Month, -Day, -Year)

write_csv(air1, "~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/air1.csv")
#Manually added some blank rows in the middle.

write_excel_csv(air2, "~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/air2.csv")
#Manually save air2.csv as excel, change date column to text, and add some empty rows.


gs <- gs_title("air temperature")
gs_edit_cells(gs, input = air2temp)

#Alternatively:

write_csv(air2temp, "~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/air temperature.csv")
