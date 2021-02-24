### Today in class we are going to learn how to join data together. ###
### Created by Ruth Vergara Reyes ###
### Updated on: 2021-02-22 ###
############################################################################

##### Load Libraries #####
library(tidyverse)
library(here)

##### Load Data #####

# Environmental data  from each site 
EnviroData <- read_csv(here("week_5","data","site.characteristics.data.csv"))

view(EnviroData)

#Thermal performance data
TPCData<-read_csv(here("week_5","data","Topt_data.csv"))

view(TPCData)

##### Data Analysis #####

EnviroData_wide <- EnviroData %>%
  pivot_wider(names_from = parameter.measured,
              values_from = values) %>%                    ### pivot the data wider ### 
  arrange(site.letter)                                     ### arrange the dataframe by site ###

view(EnviroData_wide)

FullData_left <- left_join(TPCData, EnviroData_wide) %>%   ### joined data by "site.letter" ###
  relocate(where(is.numeric), .after = where(is.character))### relocate all the numeric data after the character data

head(FullData_left)
view(FullData_left)

##### Think pair share #####

FullData_left_long <- FullData_left %>%
  pivot_longer(cols = E:substrate.cover, # the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "values", # the names of the new cols with all the column names
               values_to = "measurements") %>%  # names of the new column with all the values 
  group_by(site.letter, measurements) %>%       # group by everything we want
  summarise(mean(values),
            var(values))
view(FullData_left_long)

##### New Data #####
# Make 1 tibble

T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T1

# make another tibble
T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9))
T2

# comparing the join #

left_join(T1, T2)    ## Joining, by = "Site.ID"  leads to an NA in the pH column
right_join(T1, T2)   ## Joining, by = "Site.ID"  leads to an NA in the temp. column
inner_join(T1, T2)   ## Joining, by = "Site.ID"  leads to only having the data that have information on it, we will NOT have NAs 
full_join(T1,T2)     ## Joining, by = "Site.ID"  leads to joining both data sets even if there is missing data, we will have some NAs
semi_join(T1,T2)     ## Joining, by = "Site.ID"  shows everything that is consistent and only keeps the data that is full
anti_join(T1,T2)     ## Joining, by = "Site.ID"  shows you what data is missing other row information

### Todays R package ###
install.packages("cowsay")
library(cowsay)

say("hello", by = "shark")
say("hello", by = "cat")
say("hello", by = "poop")
say("practice R", by = "yoda")
