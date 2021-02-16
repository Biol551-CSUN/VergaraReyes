### This is mondays group work
### Created by: Ruth Vergara Reyes
### Last updated on: 2021-02-15  ###


##### Load Libraries #####

library(palmerpenguins)
library(tidyverse)
library(here)
library(calecopal)

#### Load Data ####
glimpse(penguins)

########################################

### LabData1 will calculate the mean and variance of body mass by species, island, and sex without any NAs ###
LabData1<- penguins %>%                                ### we are using penguin data ###
  drop_na(sex, island, species) %>%                    ### this line of code removes NAs from the sex column ###
  group_by(island, sex, species) %>%                   ### grouping each calculation by island, sex, and species ###
  summarise(mean_bill_length = mean(bill_length_mm),
            max_bill_length = max(bill_length_mm))     ### calculating the mean and max of the bill_length_mm ###

### LabData2 will filter out male penguins, and calculate the log body mass and group them ###
LabData2 <- penguins %>%
  drop_na(species, island, sex) %>%                    ### This line of code removes NAs from data ###
  filter(sex != "male") %>%                            ### This removes data that was taken from male penguins ###
  group_by(species, island, sex) %>%                   ### Groups data in three different categories ###
  summarise(log_body_mass = log(body_mass_g)) %>%      ### Calculated what the log of the body_mass_g was and added it to the data file ### 

### the following is how I made my plot using the LabData2 ###
ggplot(mapping = (aes(x = sex, 
                      y = log_body_mass,
                      color = island)))+
     geom_boxplot(show.legend = FALSE)+                ### Code I used to make a boxplot ###
  labs(title = "Island vs. Female Body Mass",
        x = "",
        y = "Body Mass (log)")+                        ### Code I used to customize my axis titles ###
  facet_wrap (~island)+                                ### Code I used to create a facet wrap ###
  theme_minimal()+
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 18))+              ### Code I used to customize my font sizes ###
  scale_color_manual(values = cal_palette(name = "tidepool", type = "continuous"))  ### Code used to customize my colors in my plot ###

## the following code is how i saved my plot as a png ###
ggsave(here("week_4", "output", "island_vs_female_body_mass_plot.png"),
       width = 5, height = 3)

