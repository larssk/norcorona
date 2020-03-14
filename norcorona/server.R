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
library(stringr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    fetch_data <- reactive({
        #df <- read_csv("./data/vg.csv")
        #return(df)
        
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
    
    time_series_totals <- reactive({
        
        df <- fetch_data()
        
        by_date <- df %>%
            group_by(date) %>%
            tally() %>%
            rename(daily=n) %>%
            mutate(cumsum = cumsum(daily))
        
        
        return(by_date)
    })

    
    summary_county <- reactive({
        
        df <- fetch_data()
        
        by_county <- df %>%
            group_by(county) %>%
            tally() %>%
            rename(totals=n)
        print(by_county)
                
        population <- population() %>%
            select(-short)
        print(population)
        
        out <- left_join(by_county, population, by="county") %>%
                mutate(totals_by_pop = round(1000 * totals / population, 2))

        print(out)
        
        return(out)
    })
    
    summary_municipality <- reactive({
        
        df <- fetch_data()
        print(head(df))
        
        by_muni <- df %>%
            group_by(municipality) %>%
            tally() %>%
            rename(totals=n)
        print(by_muni)
        
        #population <- population() %>%
        #    select(-short)
        #print(population)
        
        #out <- left_join(by_muni, population, by="county") %>%
        #    mutate(totals_by_pop = round(1000 * totals / population, 2))
        
        
        return(by_muni)
    })
    
    population <- reactive({
        df <- read_csv("./data/populasjon.csv")
        return(df)
    })

    compile_table_muuni <- reactive({
        
        
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

    output$fullTable = DT::renderDataTable({
        compile_table()
        },
        rownames = FALSE,
        options = list(dom = 't')
    )

    
    
    output$tableSummaryCounty = DT::renderDataTable({
        summary_county()
        },
        rownames = FALSE,
        options = list(dom = 't')
    )

    output$tableSummaryMunicipality = DT::renderDataTable({
        summary_municipality()
        },
        rownames = FALSE,
        options = list(dom = 't')
    )
    
        
    output$norgePlot <- renderPlot({
        
        df_norge  <- time_series_totals()
        df_fylker <- time_series_county()
        population <- population() %>%
            select(-short)
        
        df_fylker <- left_join(df_fylker, population, by="county") %>%
            mutate(cumsum_by_pop = 1000 * cumsum / population) %>%
            mutate(daily_by_pop = 1000 * daily / population) 
                
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
                    geom_line(data = df_fylker, mapping = aes(date, cumsum_by_pop, col=county)) +
                    geom_point(data = df_fylker, mapping = aes(date, cumsum_by_pop, fill=county, size=cumsum_by_pop), col="black", pch=21) + 
                    
                    #geom_line(data = df_fylker, mapping = aes(date, cumsum, col=county)) +
                    #geom_point(data = df_fylker, mapping = aes(date, cumsum, fill=county, size=cumsum), col="black", pch=21) + 
                    guides(size=FALSE)
                    
            }
            
            p <- p +
                scale_fill_brewer("", type="qual", palette = "Set3") + 
                scale_color_brewer("", type="qual", palette = "Set3")
            
        }
        p <- p + xlab("Dato") + ylab("Antall smittede") +
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
        
    })
    

})
