### This lesson we will learn how to make a function ###
### Created by: Ruth Vergara Reyes ###
### Updated on: 2021-03-24 ###

##### Load Libraries #####
library(tidyverse)
library(palmerpenguins)
library(calecopal)
##### Functions #####

df <- tibble::tibble(
  a = rnorm(10), # draws 10 random values from a normal distribution
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10))

head(df)

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)))

df<-df %>%
  mutate(a = (a-min(a, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(a, na.rm = TRUE)),
         b = (b-min(b, na.rm = TRUE))/(max(a, na.rm = TRUE)-min(b, na.rm = TRUE)),
         c = (c-min(c, na.rm = TRUE))/(max(c, na.rm = TRUE)-min(c, na.rm = TRUE)),
         d = (d-min(d, na.rm = TRUE))/(max(d, na.rm = TRUE)-min(d, na.rm = TRUE)))

rescale01 <- function(x) {
  value<-(x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  return(value)
  }
df %>%
  mutate(a = rescale01(a),
         b = rescale01(b),
         c = rescale01(c),
         d = rescale01(d))

### Lets make a function to convert degrees fahrenheit to celcius #
temp_C <- (temp_F-32)*5/9 

fahrenheit_to_celsius <- function(temp_F) { 
  temp_C <- (temp_F - 32) * 5 / 9 
  return(temp_C)
}

# testing the function #
F_to_C(32)

### making plots into functions ###

cal <- cal_palette("lake",3, type = "discrete")

ggplot(penguins, aes(x = body_mass_g, y = bill_length_mm, color = island))+
  geom_point()+
  geom_smooth(method = "lm")+ # add a linear model
  scale_color_manual("Island", values= cal)+  # use pretty colors and change the legend title
  theme_bw()

### setting up a new function for plots ###

myplot<-function(data, x, y){ 
  cal <- cal_palette("lake",3, type = "discrete") # my color palette 
ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=cal)+   # use pretty colors and change the legend title
    theme_bw()
}

myplot(data = penguins, x = body_mass_g, y = flipper_length_mm)


### addinf defualts ###
myplot<-function(data = penguins, x, y){
  cal <- cal_palette("lake",3, type = "discrete") # my color palette 
  ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=cal)+   # use pretty colors and change the legend title
    theme_bw()
}

# now we can just write: #
myplot(x = body_mass_g, y = flipper_length_mm)

### Layering the plot ###

myplot(x = body_mass_g, y = flipper_length_mm)+
  labs(x = "Body mass (g)",
       y = "Flipper length (mm)")

# adding an if-else statement for flexibility #
a <- 4
b <- 5

if (a > b) { # my question
  f <- 20 # if it is true give me answer 1
} else { # else give me answer 2
  f <- 10
}

f

### back to ploting ###

myplot<-function(data = penguins, x, y ,lines=TRUE ){ # add new argument for lines
ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
    geom_point()+
    geom_smooth(method = "lm")+ # add a linear model
    scale_color_manual("Island", values=cal)+   # use pretty colors and change the legend title
    theme_bw()
}

### if-else plotting ###
myplot<-function(data = penguins, x, y, lines=TRUE ){ # add new argument for lines
if(lines==TRUE){
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      geom_smooth(method = "lm")+ # add a linear model
      scale_color_manual("Island", values=cal)+   # use pretty colors and change the legend title
      theme_bw()
}
 else{
    ggplot(data, aes(x = {{x}}, y = {{y}} , color = island))+
      geom_point()+
      scale_color_manual("Island", values=cal)+   # use pretty colors and change the legend title
      theme_bw()
  }
}

myplot(x = body_mass_g, y = flipper_length_mm)
myplot(x = body_mass_g, y = flipper_length_mm, lines = FALSE) 
