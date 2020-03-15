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
