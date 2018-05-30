#Exercise


library(janitor)
library(skimr)

library(visdat)
library(skimr)
library(lubridate)
library(tidyverse)


# Load data
library(fs)
library(readr) #Part of tidyverse


air1 <- 
  read_csv(path_expand("~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/air1.csv"))

# Look at data

summary(air1)

require(ggplot2)

air1 %>% 
  ggplot(aes(x = `Date?`, y = `Temp. in F`)) + geom_point()


# Back up - lets clean this up a bit.
require(janitor)


air1 %>% 
  clean_names()

air1 <- air1 %>% 
  clean_names() %>% 
  rename(ozone = ozone_layer,
         solar = solar_r,
         wind = wind_speed_miles_hour,
         temp = temp_in_f)


#Now lets look at it more.
library(skimr)

air1 %>% skim()


#What's going on with the missings?

library(visdat)

air1 %>% vis_dat()

#Lots of missing!

air1 %>% 
  remove_empty(which = "rows") %>% 
  vis_dat() #looking for a pattern, don't see any

air1 <- air1 %>% 
  remove_empty(which = "rows")

#Check again
skim(air1)  #much better

#What about the date?
require(lubridate)

air1 <- air1 %>% 
  mutate(date = mdy(date))

air1 %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  skim()



# Add additional years ------------------------------------------------------------------------
library(readxl)

air2 <- read_excel(path_expand("~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/air2.xlsx"))

#air2 <- read_excel(path_expand("~/Google Drive/SI/DataScience/Data Science Training/RA Training/RA Training Wk 2 - Data Import & Cleaning/air2.xlsx"), na = "NA")

skim(air2)

air2 %>% 
  clean_names()

air2 <- air2 %>% 
  clean_names() %>% 
  rename(ozone = ozone_layer,
         solar = solar_r,
         wind = wind_speed_miles_hour,
         date = enter_date)

air2 %>% vis_dat()

air2 %>% 
  ggplot(aes(x = date, y = temp)) + geom_point()

#temp is "NA" characters, not true NA

air2 %>% 
  replace_with_na(replace = list(temp = "NA"))

air2 %>% skim()

air2 %>% vis_dat()

air2 %>% 
  remove_empty(which = "cols") %>% 
  vis_dat()

air2 <- air2 %>% 
  remove_empty(which = "cols")

air2 <- air2 %>% 
  mutate(date = excel_numeric_to_date(date))



# Join years ----------------------------------------------------------------------------------

air1 %>% distinct(year(date))
air2 %>% distinct(year)

air1 %>% count(year(date))
air2 %>% count(year(date))


air <- bind_rows(air1, air2)

vis_dat(air)

air %>% 
  mutate(year = year(date)) %>% 
  count(year)

air %>% 
  get_dupes(date)


# Add temperature

library(googlesheets)

gs <- gs_title("air temperature")

airtemp <- gs_read(gs)

airtemp <- airtemp %>% 
  clean_names()

vis_dat(airtemp)


air2 %>% get_dupes(date)

airtemp %>% get_dupes(date)

left_join(air2, airtemp)

air2temp <- left_join(air2, airtemp)

air2temp <- vis_dat()

air <- bind_rows(air1, air2temp)

air %>% vis_dat()

air %>% 
  ggplot(aes(x = date, y = temp)) + geom_miss_point()

air %>% 
  get_dupes(date)

#someone used celsius!

air2temp %>% 
  mutate(temp = )