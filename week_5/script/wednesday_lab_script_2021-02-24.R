### This is Wednesday's lab script. 
### Created by: Ruth Vergara Reyes
### Updated on: 2021-02-24 

################################################################################

##### Load Libraries #####
library(tidyverse)
library(here)
library(lubridate)
library(calecopal)

##### Load Data #####
CondData <- read_csv(here("week_5", "data", "CondData.csv"))
DepthData <- read_csv(here("week_5", "data", "DepthData.csv"))

view(CondData)
view(DepthData)

################################################################################

##### Cleaning up CondData and DepthData #####
ConData <- CondData %>% 
  mutate(DateTime = ymd_hms(date))%>%                                
  mutate(DateTime - seconds(2)) %>%                                             ### How I was able to get the date in the right format ###
  rename("Old_Date" = "date",
         "na_date" = "DateTime",
         "date" = "DateTime - seconds(2)")%>%                                   ### How I renamed my columns ###
  select(-Old_Date, -na_date)                                                   ### How I got rid of columns ### 

DepthData <- DepthData %>%
  mutate(date = ymd_hms(date))                                                  ### How I ensured all the dates are in correct format ###

view(ConData)
view(DepthData)

##### Joining the two dataframes #####

FullData <- inner_join(ConData, DepthData) 

view(FullData)

##### Cleaning up the joined Dataframe #####

FullData_summary <- FullData %>%
  mutate(time_hour = hour(date)) %>%                                            ### How I extracted the hour ### 
  mutate(time_minute = minute(date)) %>%                                        ### How I extracted the minutes ###
  group_by(time_hour, time_minute) %>%                                          ### How I grouped them ###
  summarise(mean_date = mean(date),
            mean_depth = mean(Depth),
            mean_temp. = mean(TempInSitu),
            mean_salinity = mean(SalinityInSitu_1pCal))                         ### How I got the mean of the Date, Depth, Temperature, and Salinity ###
  
view(FullData_summary)

################################################################################
 
##### plot for the new dataframe #####

ggplot(data = FullData_summary,
       mapping = (aes(x = mean_salinity,
                      y = mean_temp.,
                      color = mean_depth)))+                                    ### Code I used to define my axis ###
  geom_point()+
  geom_smooth(show.legend = FALSE)+                                             ### Code I used to make a graph ###
  labs(title = "Temperature vs. Depth",
       x = " Salinity",
       y = "Depth")+                                                            ### Code I used to customize my axis titles ###
  theme_minimal()+                                                              ### Code for my theme ###
  theme(axis.title = element_text(size = 13),
        title = element_text(size = 18),
        legend.title = element_text(size = 10))                                 ### Code I used to customize my font sizes ###
  

##### the following code is how i saved my plot as a png #####

ggsave(here("week_5", "output", "Temp._vs_Depth_plot.png"),
       width = 5, height = 3)
