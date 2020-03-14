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
    "Agder" = "Agder", 
    "Innlandet" = "Innlandet", 
    "Møre og Romsdal" = "Møre og Romsdal", 
    "Nordland" = "Nordland", 
    "Oslo" = "Oslo", 
    "Rogaland" = "Rogaland", 
    "Vestfold og Telemark" = "Vestfold og Telemark", 
    "Troms og Finnmark" = "Troms og Finnmark", 
    "Trøndelag" = "Trøndelag",
    "Vestland" = "Vestland", 
    "Viken" = "Viken", 
    "Svalbard" = "Svalbard"
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
                           'input_fylke', 'Fylke(r)', fylke_name, multiple=TRUE, selectize=TRUE
                       ),
                       selectInput(
                           'input_style', 'Visualisering', 
                           list("Daglige smittede" = "daily", "Kumulativt" = "cumulative", "Stable fylker" = "stack_cols"),
                           selected = c("daily", "cumulative"),
                           multiple=TRUE, selectize=TRUE
                       )

                       
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
        ), 
        fluidRow(
            column(12,
                   DT::dataTableOutput("tableSummaryCounty")
            )
        ),
        fluidRow(
            column(12,
                   DT::dataTableOutput("tableSummaryMunicipality")
            )
        )
    )
)
