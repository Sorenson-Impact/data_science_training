library(tidyverse)


# Load data -----------------------------------------------------------------------------------

library(fs)
library(readr)

air1 <- read_csv(path_expand("~/Desktop/air1.csv"))

air1

summary(air1)

#visualize
require(ggplot2)

air1 %>% 
  ggplot(aes(x = `Date?`, y = `Temp. in F`)) + geom_point()


library(janitor)

air1 <- air1 %>% 
  clean_names()

air1 <- air1 %>% 
  rename(ozone = ozone_layer,
         solar = solar_r,
         wind = wind_speed_miles_hour,
         temp = temp_in_f)

library(skimr)

air1 %>% skim()

library(visdat)

air1 %>% vis_dat()

air1 <- air1 %>% 
  remove_empty(which = "rows") 

skim(air1)

library(lubridate)

air1 <- air1 %>% 
  mutate(date = mdy(date))

air1 %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  skim

air1 %>% 
  get_dupes(date)


# Add years -----------------------------------------------------------------------------------

library(readxl)

air2 <- read_excel(path_expand("~/Desktop/air2.xlsx"))

air2 <- read_excel(path_expand("~/Desktop/air2.xlsx"), na = "NA")

air2 <- air2 %>% 
  clean_names %>% 
  rename(ozone = ozone_layer,
         solar = solar_r,
         wind = wind_speed_miles_hour,
         date = enter_date)

skim(air2)
vis_dat(air2)

library(naniar)

air2 <- air2 %>% 
  replace_with_na(replace = list(temp = "NA"))
  
air2 <- air2 %>% 
  remove_empty(which = "cols") 

air2 <- air2 %>% 
  mutate(date = excel_numeric_to_date(date))


# Join years ----------------------------------------------------------------------------------

air2 %>% 
  mutate(year = year(date)) %>% 
  count(year)

air <- bind_rows(air1, air2) 

air %>% 
  get_dupes(date)


# Get temperatures ----------------------------------------------------------------------------

library(googlesheets)

gs <- gs_title("air temperature")

airtemp <- gs_read(gs) %>% 
  clean_names()

airtemp %>% skim()
airtemp %>% vis_dat()

air2temp <- left_join(air2, airtemp)

vis_dat(air2temp)

air2temp <- air2temp %>% 
  mutate(temp = temp * 1.8 + 32)

air <- bind_rows(air1, air2temp)

vis_dat(air)

air %>% 
  get_dupes()

air %>% 
  ggplot(aes(x = date, y = temp)) + geom_point()

air <- air %>% 
  distinct()




















