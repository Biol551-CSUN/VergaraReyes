### Today we will learn to plot penguin data differently. ###
### Created by: Ruth VErgara Reyes ###
### Updated on: 2021-02-15 ###


### Load Libraries #####
library(palmerpenguins)
library(tidyverse)
library(here)

### Load Data #####
# the data we are going to use is in the package called penguins
glimpse(penguins)

### DATA ####

head(penguins)
filter(.data = penguins, sex == "female")          
filter(.data = penguins, year == "2008")
filter(.data = penguins, body_mass_g > "5000")
#### the three lines above show us how we code to filter out certain data ###

filter(.data = penguins, sex == "female", body_mass_g >4000)
filter(.data = penguins, sex == "female" & body_mass_g >4000)
### code above shows two different ways to write the same code ###

filter(.data = penguins, year == 2008 | year == 2009)
filter(.data = penguins, island !="Dream")
filter(.data = penguins, species == "Adelie" & species == "Gentoo")

data2<-mutate(.data = penguins, 
              body_mass_kg = body_mass_g/1000)
View(data2)
### we converted mass to kg and added that as a new column ###

data2<-mutate(.data = penguins, 
              body_mass_kg = body_mass_g/1000,
              bill_length_depth = bill_length_mm/bill_depth_mm)    
view(data2)
### we added another new column to data set that calculated the ratio of bill length to depth ### 

data2<- mutate(.data = penguins,
               after_2008 = ifelse(year>2008, "After 2008", "Before 2008"))
View(data2)

data2<- mutate(.data = penguins,
               flipper_length_body_mass = flipper_length_mm +body_mass_g)
view(data2)

data2<- mutate(.data = penguins,
               sex_cap = ifelse(sex == "male", "Male", "Female"))
view(data2)

penguins %>%                               # use penguin dataframe
  filter(sex == "female") %>%              #select females
  mutate(log_mass = log(body_mass_g)) %>%     #calculate log biomass
  select(species, island, sex, log_mass)

penguins %>%  
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE))
### line above codes for: calculation of mean flipper length excluding any NA's ###

penguins %>% # 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE),
            min_flipper = min(flipper_length_mm, na.rm=TRUE))
### code above layers calculations ###

penguins %>%
  group_by(island) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))
### how we would do summaries in groups (in this case we grouped them by islands)

penguins %>%
  group_by(sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))
penguins %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))
### grouping them by island and sex ###

penguins %>%
  drop_na(sex) %>%                           #### this line of code removes NA's from the sex column ###
  group_by(island, sex, species) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()
