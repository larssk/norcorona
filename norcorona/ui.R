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
            column(3,
                   wellPanel(
                       checkboxGroupInput("checkGroup", 
                                          "Hæ?",
                                          choices = list("Fylker" = "fylker", 
                                                         "Totalt" = "norge"),
                                          selected = c("norge")
                       )
                   )
            ),
            column(9,
                   plotOutput("norgePlot")
            )
        ),
        fluidRow(
            column(12,
                DT::dataTableOutput("norgeTable")
            )

        )
    )
)
