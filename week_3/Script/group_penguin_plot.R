### This is our groups penguin plot. 
### Contributors: Ruth Vergara Reyes, Amanda Chiachi, Shane Jordan, Kevin Candray
### Updated on: 2021-02-10
######################################################

#### Load Libraries ####

library(palmerpenguins)
library(tidyverse)
library(here)
library(calecopal)
library(dplyr)

###########################

glimpse(penguins)
view(penguins)

##########################

###### Plot Data #########

ggplot(data = penguins,                     ### using island and body mass as the data we are going to graph.
       mapping = aes(x = factor(island),
                     y = body_mass_g,
                     color = island
                     ))+
  geom_violin(show.legend = FALSE)+         ### we are going to make a facet_wrap violin graph
  facet_wrap(~species)+
  geom_smooth()+
  labs(x = "",
       y = "Body Mass (g)",
       title = "Body Mass of Different Penguin Species on Three Islands")+
  guides(color = FALSE,
         fill = FALSE)+
  theme_light()+
  theme(axis.title = element_text(size = 15))+
scale_color_manual(values = cal_palette(name = "bigsur2", type = "continuous"))+
  scale_y_continuous(limits = c(2500,6500))

#########################################################

ggsave(here("week_3", "output", "lab_penguin_plot.png"),
       width = 7, height = 5)
