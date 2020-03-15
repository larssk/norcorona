#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(ggplot2)
library(lubridate)
library(tibble)
library(dplyr)
library(tidyr)
library(DT)
library(curl)
library(jsonlite)
library(shinydashboard)
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    source("server_dataHandler.R", local=TRUE)$value
    source("server_dataTransformer.R", local=TRUE)$value
    
    source("server_renderPlot.R", local=TRUE)$value
    source("server_renderBox.R", local=TRUE)$value
    source("server_renderTable.R", local=TRUE)$value

    

 
    
 
    
})
