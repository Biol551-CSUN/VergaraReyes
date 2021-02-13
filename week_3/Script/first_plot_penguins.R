### This is my first time making a plot on R. I am learning how to use ggplot.
### Created by: Ruth Vergara Reyes
### Created on: 2021-02-08
### Last edited: 2021-02-09
############################################################

##### Load Library ####

library(tidyverse)
library(palmerpenguins)

#### Data Analysis ####

glimpse(penguins)
ggplot(data=penguins, 
    mapping = aes(x = bill_depth_mm,
                  y = bill_length_mm,
                  color = species,
                  size = body_mass_g,
                  alpha = flipper_length_mm))+ 
  geom_point()+
   labs(title = "Bill depth and length",
        subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
        x = "Bill depth (mm)", 
        y = "Bill length (mm)",
        color = "Species",
        caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_color_viridis_d()

ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm))+
  geom_point()+
  facet_grid(species~sex)

ggplot(penguins,
       aes(x = bill_depth_mm,
           y = bill_length_mm))+
  geom_point()+
  facet_wrap(~ species, ncol = 2)


ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,))+
  geom_point()+
  scale_color_viridis_d()+
  facet_grid(species~sex)+
  guides(color = FALSE)
