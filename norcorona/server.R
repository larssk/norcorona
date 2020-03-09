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
    
    compile_table <- reactive({
        df <- fetch_data()
        
        df <- df %>% 
            select(-daily) %>%
            pivot_wider(
                names_from = c(fylke), 
                values_from = c(cumsum)
            ) %>%
            arrange(desc(date)) %>%
            select(date, norge, everything())

        return(df)
    })

    output$fullTable = DT::renderDataTable({
        compile_table()
    },
    rownames = FALSE,
    options = list(dom = 't'), 
    colnames = c('Dato', 'Norge', 'Agder', 'Innlandet', 'Møre og Romsdal', 'Norland', 
                 'Oslo', 'Rogaland', 'Vestfold og Telemark', 'Troms og Finnmark', 'Trøndelag', 
                 'Vestland', 'Viken')
    )
    
    output$norgePlot <- renderPlot({
        
        df_norge  <- filter_norge()
        df_fylker <- filter_fylker()
        
        # Filter dataframe on selection
        if( length(input$input_fylke) > 0 ) {
            df_fylker <- df_fylker %>%
                filter(fylke %in% input$input_fylke) %>%
                drop_na(cumsum)
        }
        
        p <- ggplot()
        
        # Plot totals of entire Norway
        if( length(input$input_fylke) == 0 ) {
            
            # Column for daily incidents
            if("daily" %in% input$input_style) {
                p <- p + 
                    geom_col(data = df_norge, aes(date, daily), fill=2)
            }
            
            # Stack by county
            if("stack_cols" %in% input$input_style) {
                p <- p + 
                    geom_col(data = df_fylker, mapping = aes(date, cumsum, fill=fylke)) +
                    scale_fill_brewer("", type="qual", palette = "Set3")
            }
            
            if("cumulative" %in% input$input_style) {
                p <- p + 
                    geom_point(data = df_norge, mapping = aes(date, cumsum, size=cumsum)) + 
                    geom_line(data = df_norge, mapping = aes(date, cumsum)) +
                    guides(size=FALSE)
                    #scale_size_continuous("")
            }
            
        }
        else {
            
            if("daily" %in% input$input_style) {
                if("stack_cols" %in% input$input_style) {
                    position = "stack"
                }
                else {
                    position = "dodge2"
                }

                p <- p + 
                    geom_col(data = df_fylker, mapping = aes(date, daily, fill=fylke), col="black", position = position)
            }

            if("cumulative" %in% input$input_style) {
                p <- p + 
                    geom_line(data = df_fylker, mapping = aes(date, cumsum, col=fylke)) +
                    geom_point(data = df_fylker, mapping = aes(date, cumsum, fill=fylke, size=cumsum), col="black", pch=21) + 
                    guides(size=FALSE)
                    
            }
            
            p <- p +
                scale_fill_brewer("", type="qual", palette = "Set3") + 
                scale_color_brewer("", type="qual", palette = "Set3")
            
        }
        p <- p + xlab("Dato") + ylab("Antall smittede")
        
        p <- p + 
            theme_dark() + 
            theme(
                axis.text=element_text(size=16),
                axis.title=element_text(size=16, face="bold"), 
                legend.text = element_text(size=19)
            ) + 
            theme(legend.position="top")
        p
        
    })
    

})
