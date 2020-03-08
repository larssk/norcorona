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

fylke_name = list(
    "Agder" = "agder", 
    "Innlandet" = "innlandet", 
    "Møre og Romsdal" = "more", 
    "Norland" = "norland", 
    "Oslo" = "oslo", 
    "Rogaland" = "rogaland", 
    "Telemark og Vestfold" = "telemark", 
    "Troms og Finnmark" = "troms", 
    "Trøndelag" = "trondelag",
    "Vestland" = "vestland", 
    "Viken" = "viken"
)

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
                       selectInput(
                           'input_fylke', 'Velg fylke', fylke_name, multiple=TRUE, selectize=TRUE
                           ),
                       selectInput(
                           'input_bars', 'Stolpene', list("Daglig" = "daglig", 
                                                          "Per fylke" = "fylker"), 
                           selected = "daglig"
                                                          
                       ),
                       checkboxInput(
                           "input_cumulative", 'Kumulativ', value = TRUE
                       ), 
                       
                       
                   )
            ),
            column(9,
                   plotOutput("norgePlot")
            )
        ),
        fluidRow(
            column(12,
                DT::dataTableOutput("fullTable")
            )

        )
    )
)
