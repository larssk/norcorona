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
library(DT)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        
        # theme
        theme = shinytheme("united"),
        
        # Application title
        titlePanel("Norway Corona Stats"),
        
        fluidRow(
            column(2,
                   wellPanel(
                       checkboxGroupInput("checkGroup", 
                                          "Hæ?",
                                          choices = list("Kumulativ" = "show_cum", 
                                                         "Daglig" = "show_daily"),
                                          selected = c("show_cum", "show_daily")
                       )
                   )
            ),
            column(5,
                   plotOutput("norgePlot")
            ),
            column(5,
                   plotOutput("fylkePlot")
            )
        ),
        fluidRow(
            column(2),
            column(4,
                DT::dataTableOutput("norge")
            ),
            column(4,
                   DT::dataTableOutput("fylke")
            )
            
        )
    )
)
