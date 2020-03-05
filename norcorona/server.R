#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lubridate)
library(tibble)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        #        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        df <- read_csv("data/norge.csv") %>%
            mutate(cumsum = cumsum(daily))
        
        
        p <- ggplot(df)
        
        if (input$show_daily) {
            p <- p + geom_col(mapping = aes(date, daily), fill=2)
        }
        
        if (input$show_cum) {
            p <- p + 
                geom_point(mapping = aes(date, cumsum)) + 
                geom_line(mapping = aes(date, cumsum))
        }
    
        p <- p + xlab("Dato") + ylab("Antall smittede")
        p
        
    })

})
