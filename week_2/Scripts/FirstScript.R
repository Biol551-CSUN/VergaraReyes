### This is my first script. I am learning hot to import data
### Created by: Ruth Vergara Reyes
### Created on: 2021-02-03
#############################################################


### Load libraries ###
library(tidyverse)
library(here)


### Read in Data ###

weightData <- read.csv(here("week_2","Data","weightdata.csv"))

### Data Analysis ####

head(weightData)
tail(weightData)
View(weightData)
