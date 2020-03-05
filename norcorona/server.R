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
    
    output$norgePlot <- renderPlot({
        
        df <- read_csv("data/norge.csv") %>%
            mutate(cumsum = cumsum(daily))
        
        p <- ggplot(df)
        
        
        if ("show_daily" %in% input$checkGroup) {
            p <- p + geom_col(mapping = aes(date, daily), fill=2)
        }
        
        if ("show_cum" %in% input$checkGroup) {
            p <- p + 
                geom_point(mapping = aes(date, cumsum)) + 
                geom_line(mapping = aes(date, cumsum))
        }
        
        p <- p + xlab("Dato") + ylab("Antall smittede")
        
        p <- p + theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16, face="bold"))
        p
        
    })
    
    output$fylkePlot <- renderPlot({
        
        df <- read_csv("data/fylke.csv")
        
        df2 <- pivot_longer(df, cols = -date, names_to = "fylke", values_to = "cumsum")
        
        df3 <- df2 %>%
            filter(fylke %in% c("vestland", "viken", "oslo"))
        
        p <- ggplot(df3, aes(date, cumsum, col=fylke)) +
            geom_line() + geom_point()
        
        p <- p + xlab("Dato") + ylab("Antall smittede")
        p <- p + theme(axis.text=element_text(size=16),
                       axis.title=element_text(size=16, face="bold"), 
                       legend.text = element_text(size=14),
                       legend.position="top") +
            labs(col="")
        p
        
    })
    
})
