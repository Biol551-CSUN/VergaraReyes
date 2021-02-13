### Today we are going to plot penguin data ###
### Created by: Ruth Vergara Reyes
### Updated on: 2021-02-10 

### Load Libraries ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(calecopal)

### Load data ###
# The data is part of the package and is called penguins.
glimpse(penguins)


### Plot data ###

ggplot(data = penguins,
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     group = species,
                     color = species))+
   geom_point()+
  geom_smooth()+
     labs(x = "Bill depth (mm)",
          y = "Bill length (mm)")+
  scale_color_manual(values = cal_palette(name = "kelp2", type = "continuous"))+
  theme_classic()+
  theme(axis.title = element_text(size = 15,
                                  color = "blue"),
        panel.background = element_rect(fill = "linen"))+
  ggsave(here("week_3", "output", "penguin.png"),
         width = 7, height = 5)


# coord_flip()  # flips the x and y axis
# scale_color_viridis_d() is a color pallet that is colorblind friendly. 
# scale_x_continuous(breaks = c(14,17,21),
                     # labels = c("low", "medium", "high"))
# code lines above are used to be able to change/ customize axis scales.