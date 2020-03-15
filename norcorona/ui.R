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
library(shinydashboard)

fylke_name <-  list(
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
    "Viken" = "Viken"
)
    



dashboardPage(
    dashboardHeader(
        title = "Koronadash"
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Oversikt", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Fylkesoversikt", tabName = "county", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard", 
                    fluidRow(
                        infoBoxOutput("totalInfected"), 
                        infoBoxOutput("todayInfected"), 
                        infoBoxOutput("totalDiseased")
                    ),
                    
                    
                    fluidRow(
                        box(
                            plotOutput("norwayPlot"), width = 12
                        )
                    ), 
                    
                    fluidRow(
                        box(
                            DT::dataTableOutput("countyTable")
                        ), 
                        
                        box(
                            DT::dataTableOutput("municipalityTable")
                        )
                        
                    )
            ), 
            
            tabItem(tabName = "county", 
                    
                    fluidRow(

                        box(
                            plotOutput("countyPlot")
                        ),
                        
                        box(
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
                    
                    fluidRow(
                        box(
                            DT::dataTableOutput("fullTable"), width = 12
                        )
                    )
            )
        )
    )
)
