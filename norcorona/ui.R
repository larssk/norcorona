#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        
        # theme
        theme = shinytheme("united"),
        
        # Application title
        titlePanel("Norway Corona Stats"),
        
        fluidRow(
            column(6,
                   plotOutput("norgePlot")
            ),
            column(6,
                   plotOutput("fylkePlot")
            )
            
        ), 
        
        fluidRow(
            column(3,
                   wellPanel(
                       checkboxGroupInput("checkGroup", 
                                          h3("Checkbox group"), 
                                          choices = list("Kumulativ" = "show_cum", 
                                                         "Daglig" = "show_daily"),
                                          selected = c("show_cum", "show_daily")
                       )
                   ) 
            )
        )
    )
)
