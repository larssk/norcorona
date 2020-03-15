### Render tables for UI ###
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


output$municipalityTable = DT::renderDataTable({
    compile_table_municipality()
},
rownames = FALSE,
options = list(dom = 't')
)


### Compile tables for rendering ###
compile_table_municipality <- reactive({
    
    df_muni <- totals_by_municipality() %>%
        arrange(desc(n_infected)) %>%
        replace_na(list(municipality='Ukjent'))

    population_muni <- fetch_population_municipality()
    
    out <- left_join(df_muni, population_muni, by="municipality") %>%
        mutate(totals_by_pop = round(1000 * n_infected / population, 2)) %>%
        select(municipality, n_infected, totals_by_pop) %>%
        rename(`Smittede`=n_infected, Kommune=municipality, `per 1000`=totals_by_pop)

    
    return(out)
})


compile_table_county <- reactive({
    
    df_county <- totals_by_county() %>%
        arrange(desc(n_infected)) %>%
        replace_na(list(county='Ukjent'))
    
    population_county <- fetch_population_county()
        
    out <- left_join(df_county, population_county, by="county") %>%
        mutate(totals_by_pop = round(1000 * n_infected / population, 2)) %>%
        select(county, n_infected, totals_by_pop) %>%
        rename(`Smittede`=n_infected, Fylke=county, `per 1000`=totals_by_pop)
    
    return(out)
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
