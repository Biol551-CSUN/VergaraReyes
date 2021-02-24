### This script we will be practicing how to use lubridate dates and times. 
### Created by: Ruth Vergara Reyes
### Updated on: 2021-02-24

#############################################################################

##### Load Libraries #####
library(tidyverse)
library(here)
library(lubridate)

#############################################################################

### practice ###

now()                                           ### what time is it now ###
now(tzone = "EST")                              ### what time is it on the east coast ###
now(tzone = "GMT")                              ### what time in GMT ###
today()                                         ### what is todays date ###
today(tzone = "GMT")                            ### what date is it in GMT ###
am(now())                                       ### is it morning? ###
leap_year(now())                                ### is it a leap year? ###

ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")

datetimes<-c("02/24/2021 22:22:20",
             "02/25/2021 11:21:10",
             "02/26/2021 8:01:52")             ### maked a character string ###

datetimes <- mdy_hms(datetimes)                ### convert to datetimes ### 
month(datetimes, label = TRUE)
month(datetimes, label = TRUE, abbr = FALSE)   ###Spell it out ###

day(datetimes)                                 ### extract day ###
wday(datetimes, label = TRUE)                  ### extract day of week ###
hour(datetimes)                                ### extract hour ###
minute(datetimes)                               
second(datetimes)

datetimes + hours(4)                           ### this adds 4 hours ###
datetimes + days(2)                            ### this adds 2 days ###

round_date(datetimes, "minute")                ### round to nearest minute ###
round_date(datetimes, "5 mins")                ### round to nearest 5 minute

##### THink Pair Share #####
ConData <- read_csv(here("week_5", "data", "CondData.csv"))
view(ConData)

CondData_clean <- ConData %>%
  mutate(DataTime = ymd_hms(date))
view(CondData_clean)
