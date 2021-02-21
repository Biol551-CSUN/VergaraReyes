### Today we will be using new data to make plots. We will be practicing tidy with the new data.#
### Created by: Ruth Vergara Reyes ###
### Updated on: 2021-02-20 ###

##### Load Libraries #####
library(tidyverse)
library(here)

##### Load Data #####
ChemData <- read_csv(here("week_4", "data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

##### Data Analysis #####
ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) %>%        ### filters out everything that is not a complete row #
  separate(col = Tide_time,            ### choose the column we will split ###
           into = c("Tide", "Time"),   ### name of new columns ###
           sep = "_")                  ### what we will separate by ###
head(ChemData_clean)
view(ChemData_clean)

ChemData_long <-ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. This says select the temp to percent SGD cols
               names_to = "Variables", # the names of the new cols with all the column names
               values_to = "Values") # names of the new column with all the values
view(ChemData_long)

ChemData_long %>%
  group_by(Variables, Site, Zone, Tide) %>% # group by everything we want
  summarise(Param_means = mean(Values, na.rm = TRUE), # get mean
            Param_vars = var(Values, na.rm = TRUE), 
            Param_sd = sd(Values, na.rm = TRUE)) # get variance

ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+
  geom_boxplot()+
  facet_wrap(~Variables, scales = "free")

ChemData_wide<-ChemData_long %>%
  pivot_wider(names_from = Variables, # column with the names for the new columns
              values_from = Values) # column with the values
View(ChemData_wide)

ChemData_clean<-ChemData %>%
  filter(complete.cases(.))  #filters out everything that is not a complete row
separate(col = Tide_time, # choose the tide time col
         into = c("Tide","Time"), # separate it into two columns Tide and time
         sep = "_", # separate by _
         remove = FALSE)
View(ChemData_clean)


ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% #filters out everything that is not a complete row
  separate(col = Tide_time, # choose the tide time col
           into = c("Tide","Time"), # separate it into two columns Tide and time
           sep = "_", # separate by _
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd, # the cols you want to pivot. This says select the temp to percent SGD cols  
               names_to = "Variables", # the names of the new cols with all the column names 
               values_to = "Values") %>% # names of the new column with all the values 
  group_by(Variables, Site, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Variables, 
              values_from = mean_vals) %>% # notice it is now mean_vals as the col name
  write_csv(here("Week_4","output","summaryforlecturepractice.csv"))  # export as a csv to the right folder
