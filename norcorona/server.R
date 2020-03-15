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
    
    fetch_data <- reactive({
                
        con <- curl_fetch_memory("https://www.vg.no/spesial/2020/corona-viruset/data/norway-allCases/")
        all_cases <- jsonlite::fromJSON(rawToChar(con$content))
        
        dates <- names(all_cases)
        for(i in 1:length(all_cases)) {
            all_cases[[i]]$date = dates[i]
        }

        df <- as_tibble(bind_rows(all_cases))
        
        df <- df %>%
            mutate(municipality = str_remove(municipality, '"')) %>%
            mutate(date = ymd(date)) %>%
            mutate(date = ymd(confirmed))
        
        # returns a dataframe with a row per case
        return(df)
        
    })

    fetch_data_overview <- reactive({
        con <- curl_fetch_memory("https://www.vg.no/spesial/2020/corona-viruset/data/norway-table-overview/")
        overall <- jsonlite::fromJSON(rawToChar(con$content))

        return(
            overall$totals
        )
        
        ## $confirmed, $dead, $recovered
    })
    
    totals_by_county <- reactive({
        df <- fetch_data()
        
        infected <- df %>%
            group_by(county) %>%
            tally() %>%
            rename(n_infected = n)

        diseased <- df %>%
            drop_na(dead) %>%
            group_by(county) %>%
            tally() %>%
            rename(n_dead = n)
        
        out <- left_join(infected, diseased, by="county")
        return(out)
        
    })

    totals_by_municipality <- reactive({
        df <- fetch_data()
        
        infected <- df %>%
            group_by(municipality) %>%
            tally() %>%
            rename(n_infected = n)
        
        diseased <- df %>%
            drop_na(dead) %>%
            group_by(municipality) %>%
            tally() %>%
            rename(n_dead = n)
        
        out <- left_join(infected, diseased, by="municipality")
        return(out)
        
    })
    
    time_series_county <- reactive({

        df <- fetch_data()
        
        by_date_region <- df %>%
            group_by(date, county) %>%
            tally() %>%
            rename(daily=n) %>%
            arrange(county, date)

        splt <- by_date_region %>%
            group_by(county) %>%
            group_split(county)

        splt <- lapply(splt, function(x) {
            x$cumsum = cumsum(x$daily)
            return(x)
        })

        df <- bind_rows(splt)

        return(df)
                
    })

    time_series_municipality <- reactive({
        
        df <- fetch_data()
                
        by_date_municipality <- df %>%
            group_by(date, municipality) %>%
            tally() %>%
            rename(daily=n) %>%
            arrange(municipality, date)
        
        splt <- by_date_municipality %>%
            group_by(municipality) %>%
            group_split(municipality)
        
        splt <- lapply(splt, function(x) {
            x$cumsum = cumsum(x$daily)
            return(x)
        })
        
        df <- bind_rows(splt)
                
        return(df)
        
    })
    
        
    time_series_totals <- reactive({
        
        df <- fetch_data()
        
        by_date <- df %>%
            group_by(date) %>%
            tally() %>%
            rename(daily=n) %>%
            mutate(cumsum = cumsum(daily))
        
        
        return(by_date)
    })
    

    compile_table_municipality <- reactive({
        df_county <- totals_by_municipality() %>%
            arrange(desc(n_infected)) %>%
            replace_na(list(municipality='Ukjent')) %>%
            rename(`Smittede`=n_infected, `Døde`=n_dead, Kommune=municipality)
        return(df_county)
    })
    
    
    compile_table_county <- reactive({
        df_county <- totals_by_county() %>%
            arrange(desc(n_infected)) %>%
            replace_na(list(county='Ukjent')) %>%
            rename(`Smittede`=n_infected, `Døde`=n_dead, Fylke=county)
        return(df_county)
    })
    
    compile_table <- reactive({
        df_county <- time_series_county() %>%
            select(-daily) %>% 
            arrange(desc(date)) %>%
            pivot_wider(
                names_from = c(county), 
                values_from = c(cumsum)
            ) %>%
            fill(-date, .direction="downup")
        
        df_totals <- time_series_totals() %>%
            select(-daily) %>%
            rename(Totalt = cumsum)
        
        df <- left_join(df_totals, df_county, by="date") %>%
            arrange(desc(date)) %>%
            select(date, Totalt, Agder, Innlandet, `Møre og Romsdal`, Nordland, 
                   Oslo, Rogaland, `Vestfold og Telemark`, `Troms og Finnmark`, 
                   `Trøndelag`, Vestland, Viken)
            

        return(df)
    })

    output$todayInfected <- renderValueBox({
        
        disease_count <- nrow(
            fetch_data() %>%
                filter(date == today())
        )
        
        valueBox(
            disease_count, "Nye tilfeller i dag", icon = icon("list"),
            color = "aqua"
        )
    })    
    
    output$totalInfected <- renderValueBox({
        disease_count <- fetch_data_overview()$confirmed
        
        #disease_count <- nrow(
        #    fetch_data()
        #)
        
        valueBox(
            disease_count, "Bekreftet smittet", icon = icon("list"),
            color = "orange"
        )
    })    
    
    output$totalDiseased <- renderValueBox({
        disease_count <- fetch_data_overview()$dead
        
        #disease_count <- nrow(
        #    fetch_data() %>%
        #        drop_na(dead)
        #)
        
        valueBox(
            disease_count, "Døde", icon = icon("list"),
            color = "red"
        )
    })    
    
    output$fullTable = DT::renderDataTable({
        compile_table()
    },
    rownames = FALSE,
    options = list(dom = 't')
    )
    
    output$countyTable = DT::renderDataTable({
        compile_table_county()
    },
    rownames = FALSE,
    options = list(dom = 't')
    )
    
    output$municipalityTable = DT::renderDataTable({
        compile_table_municipality()
    },
    rownames = FALSE,
    options = list(dom = 't')
    )
    
    output$norwayPlot <- renderPlot({
        countyPlot()
    })
    output$countyPlot <- renderPlot({
        countyPlot()
    })
    
    countyPlot <- function() {
        
        df_norge  <- time_series_totals()
        df_fylker <- time_series_county()
        
        # Filter dataframe on selection
        if( length(input$input_fylke) > 0 ) {
            
            df_fylker <- df_fylker %>%
                filter(county %in% input$input_fylke) %>%
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
                    geom_col(data = df_fylker, mapping = aes(date, cumsum, fill=county)) +
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
                    geom_col(data = df_fylker, mapping = aes(date, daily, fill=county), col="black", position = position)
            }

            if("cumulative" %in% input$input_style) {
                p <- p + 
                    geom_line(data = df_fylker, mapping = aes(date, cumsum, col=county)) +
                    geom_point(data = df_fylker, mapping = aes(date, cumsum, fill=county, size=cumsum), col="black", pch=21) + 
                    guides(size=FALSE)
                    
            }
            
            p <- p +
                scale_fill_brewer("", type="qual", palette = "Set3") + 
                scale_color_brewer("", type="qual", palette = "Set3")
            
        }
        p <- p + xlab("Dato") + ylab("Bekreftet smittede") +
            scale_x_date(date_breaks = "days" , date_labels = "%d.%b")
        
        p <- p + 
            theme_dark() + 
            theme(
                axis.text=element_text(size=16),
                axis.title=element_text(size=16, face="bold"), 
                legend.text = element_text(size=19),
                axis.text.x = element_text(angle = 35, hjust = 1, vjust = .9, size=12)
                
            ) + 
            theme(legend.position="top")
        p
        
    }
    
    
})
