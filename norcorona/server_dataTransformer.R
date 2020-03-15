
    
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


   
