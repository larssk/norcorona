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
        
    ## returns a dataframe with a row per case
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


fetch_population_county <- reactive({
    df <- read_csv("./data/populasjon.csv") %>%
        group_by(county, county_code) %>%
        summarise(population = sum(population))
    
    return(df)
})

fetch_population_municipality <- reactive({
    df <- read_csv("./data/populasjon_kommuner.csv")
   
    return(df)
})
