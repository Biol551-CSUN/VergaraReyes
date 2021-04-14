### This is my first shiny app, that I will be creating. 
### Created by: Ruth Vergara Reyes
### Upodated on: 2021-04-07

###################################################

##### Load Libraries #####
library(tidyverse)
library(here)
library(kableExtra)
library(shiny)
library(lubridate)
library(shinythemes)

##### Load Data #####
baby_data <- read.csv(here("week_10", "data", "HatchBabyExport.csv"))

##### Cleaning up data #####

weight_data <- baby_data%>%                ### WEIGHT DATA ###
  filter(Activity == "Weight")%>%          ### weight is in lbs ###
  separate(col = Start.Time,               ### choose the column we will split ###
           into = c("Date", "Time"),       ### name of new columns ###
           sep = " ")%>%                   ### what we will separate by ###
  separate(col = Date,
           into = c("month", "day", "year"),
           sep = "/")%>%                   ### How I got rid of columns I don't need ###
  mutate(weight_data, Date = paste(year, month, day, sep = "-"))%>%
  select(-Time, -End.Time, 
         -Percentile, -Duration, 
         -Info, -Notes, -X,
         -month, -day, -year)




# Define UI
# Define UI#############################################################
ui <- fluidPage(
  # Some custom CSS for a smaller font for text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))
  ),
  
  fluidRow(
    column(width = 4, wellPanel(
      radioButtons("Baby Name", "Baby Name:",
                   c("Blakely",
                     "Micah")
      )
    )),
    column(width = 4,
           # In a plotOutput, passing values for click, dblclick, hover, or brush
           # will enable those interactions.
           plotOutput("birthdayPlot", height = 350,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      click = "plot_click",
                      dblclick = dblclickOpts(
                        id = "plot_dblclick"
                      ),
                      hover = hoverOpts(
                        id = "plot_hover"
                      ),
                      brush = brushOpts(
                        id = "plot_brush"
                      )
           )
    )
  ),
  fluidRow(
    column(width = 3,
           verbatimTextOutput("click_info")
    ),
    column(width = 3,
           verbatimTextOutput("dblclick_info")
    ),
    column(width = 3,
           verbatimTextOutput("hover_info")
    ),
    column(width = 3,
           verbatimTextOutput("brush_info")
    )
  )
)
# Show a plot of the city-wide distribution
mainPanel(
  column(
    6,
    h4("Weight over time for each baby"),
    plotOutput("birthdayPlot")
  )
) # /mainPanel

##################################################################    
# Application title
titlePanel("Twin Baby Stats")

# Server logic########################################################

##################################################################
server <- function(input, output) {
  rbabies <- reactive(weight_data %>%
                        filter(`Baby.Name` == input$`Baby Name`))
  output$birthdayPlot <- renderPlot({
    if (input$'Baby Name' == "Blakely") {
      rbabies() %>% 
        ggplot(aes(
          x = factor(Date),
          y = Amount
        )) +
        geom_point() +
        xlab("Date") +
        ylab("Weight of Baby") +
        theme_classic(base_size = 16)+
        theme(axis.title = element_text(size = 13),  # make axis font larger
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), # change the angle of the x axis text to see it all
              plot.title = element_text(hjust = 0.5))
    } else if (input$'Baby Name' == "Micah") {
      rbabies() %>% 
        ggplot(aes(
          x = factor(Date),
          y = Amount
        )) +
        geom_point() +
        xlab("Date") +
        ylab("Weight of Baby") +
        theme_classic(base_size = 16)+
        theme(axis.title = element_text(size = 13),  # make axis font larger
              axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), # change the angle of the x axis text to see it all
              plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$click_info <- renderPrint({
    cat("input$plot_click:\n")
    str(input$plot_click)
  })
  output$hover_info <- renderPrint({
    cat("input$plot_hover:\n")
    str(input$plot_hover)
  })
  output$dblclick_info <- renderPrint({
    cat("input$plot_dblclick:\n")
    str(input$plot_dblclick)
  })
  output$brush_info <- renderPrint({
    cat("input$plot_brush:\n")
    str(input$plot_brush)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)