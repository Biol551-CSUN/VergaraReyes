### This week's homework will involve new ways to clean up data sets. We will be practicing tidy with the new data.#
### Created by: Ruth Vergara Reyes ###
### Updated on: 2021-02-20 ###

##### Load Libraries #####
library(tidyverse)
library(here)
library(calecopal)

##### Load Data #####
ChemData <- read_csv(here("week_4", "data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

##### Data Analysis #####
ChemData_clean <- ChemData %>%
  filter(complete.cases(.)) %>%                         ### filters out everything that is not a complete row #
  separate(col = Tide_time,                             ### choose the column we will split ###
           into = c("Tide", "Time"),                    ### name of new columns ###
           sep = "_",
           remove = TRUE) %>%                           ### what we will separate by ###
  pivot_longer(cols = Salinity:Salinity:Phosphate,      ### the cols you want to pivot ###
               names_to = "Variables",                  ### what name of the new cols ###
               values_to = "Values") %>%                ### name of the new cols with the values ###
  group_by(Variables, Site, Time, Tide)%>%              ### what we want in our new data table we are making out of the data we have ###
  summarise(mean_val = mean(Values, na.rm = TRUE),      ### summaries stats we are adding to the data ###
            vars_val = var(Values, na.rm = TRUE))%>%    ### summaries stats we are adding to the data ###
write_csv(here("week_4","output","ChemData_clean.csv")) ### export as a csv into the correct folder ###

view(ChemData_clean)

#### Code to make plot ####

ggplot(data = ChemData_clean,
         mapping = aes(x = Site, 
                       y = mean_val))+                  ### values I used in my plot's x and y axis ###
  geom_count(show.legend = FALSE)+                      ### the type of plot I made using "ChemData_clean" data ###
  facet_wrap(~Variables, scales = "free")+              ### how I separated the points in the plot into Phosphate and Salinity categories ### 
  geom_smooth()+
  labs(x = "",
       y = "Mean Value",
       title = "Average Values of Phosphate and Salinity in Each Site ")+  ### New labels for my title and axis titles ###
  guides(color = FALSE,
         fill = FALSE)+
  theme_bw()+
  theme(axis.title = element_text(size = 12), 
        title = element_text(size = 15))                ### How I changed the style of my title ###

ggsave(here("week_4", "output", "lab_ChemData_clean_countplot.png"),       
         width = 7, height = 5)                         ### How I saved my plot ###

