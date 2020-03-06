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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    fetch_data <- reactive({
        df <- read_csv("./data/fylke.csv") %>% 
            pivot_longer(cols = -date, names_to = "fylke", values_to = "cumsum") %>%
            group_split(fylke)
        
        df <- lapply(
            df, function(x) {
                x$daily = abs(diff(c(x$cumsum[1]*2, x$cumsum)))
                return(x)
            }
        )
        
        df <- bind_rows(df)
        return(df)
        
    })

    filter_norge <- reactive({
        df <- fetch_data()
        df <- df %>%
            filter(fylke == "norge")
        return(df)
    })
    
    filter_fylker <- reactive({
        df <- fetch_data()
        df <- df %>%
            filter(fylke != "norge")
        return(df)
    })
    
    output$norgeTable = DT::renderDataTable({
        df <- fetch_data()
        
        df <- df %>% 
            select(-daily) %>%
            pivot_wider(
                names_from = c(fylke), 
                values_from = c(cumsum)
            ) %>%
            arrange(desc(date))
        
        return(df)
    })

    output$norgePlot <- renderPlot({
        
        df_norge  <- filter_norge()
        df_fylker <- filter_fylker()
        
        p <- ggplot()
        
        if("fylker" %in% input$checkGroup) {
            p <- p + 
                geom_col(data = df_fylker, mapping = aes(date, cumsum, fill=fylke)) +
                scale_fill_brewer(type="qual", palette = "Set3")
        }
        
        if("norge" %in% input$checkGroup) {
            p <- p + 
                geom_point(data = df_norge, mapping = aes(date, cumsum)) + 
                geom_line(data = df_norge, mapping = aes(date, cumsum)) 
            
            if(!"fylker" %in% input$checkGroup) {
                p <- p + geom_col(data = df_norge, aes(date, daily), fill=2)
            }

        }
        p <- p + xlab("Dato") + ylab("Antall smittede")
        
        p <- p + theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16, face="bold"))
        p
        
    })
    

})
