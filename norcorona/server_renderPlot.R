output$norwayPlot <- renderPlot({
    countyPlot()
})

output$countyPlot <- renderPlot({
    countyPlot()
})

countyPlot <- function() {        
    df_norge  <- time_series_totals()
    df_fylker <- time_series_county()
    #population <- fetch_population_county()
        
    #    df_fylker <- left_join(df_fylker, population, by="county") %>%
    #        mutate(cumsum_by_pop = 1000 * cumsum / population) %>%
    #        mutate(daily_by_pop = 1000 * daily / population) 
                
        # Filter dataframe on selection
        if( length(input$input_fylke) > 0 ) {
            
            df_fylker <- df_fylker %>%
                filter(county %in% input$input_fylke) %>%
                drop_na(cumsum)
            y
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
    
