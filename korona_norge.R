
library(tidyverse)
library(lubridate)
library(readr)
library(stringr)

df <- read_csv("./data/fylke.csv") %>% 
  pivot_longer(cols = -date, names_to = "fylke", values_to = "cumsum") %>%
  group_split(fylke)

df

df <- lapply(
  df, function(x) {
    x$daily = abs(diff(c(x$cumsum[1]*2, x$cumsum)))
    return(x)
  }
)

df <- bind_rows(df)

df_norge <- df %>% filter(fylke=="norge")
df_norge

ggplot(df_norge) +
  geom_col(aes(date, daily), fill=2) +
  geom_point(aes(date, cumsum)) + 
  geom_line(aes(date, cumsum))


df <- read_csv("data/norge.csv")
df

df <- df %>% 
  mutate(cumsum = cumsum(daily)) %>%
  mutate(fylke = "norge")
df

ggplot(df, aes(date, cumsum), col=1) +
  geom_line() +
  geom_col(mapping = aes(date, daily), fill=2) +
  geom_point() + 
  xlab("Dato") + ylab("Smittede")


# sumsum
df <- read_csv("./data/fylke.csv")  %>%
  pivot_longer(df, cols = -date, names_to = "fylke", values_to = "cumsum") %>%
  group_split(fylke)

df <- lapply(
  df, function(x) {
    x$daily = abs(diff(c(x$cumsum[1]*2, x$cumsum)))
    return(x)
  }
)

df <- bind_rows(df)
df

df %>% 
  select(-daily) %>%
  pivot_wider(
  names_from = c(fylke), 
  values_from = c(cumsum)
)


ggplot(df, aes(date, cumsum, col=fylke)) +
  geom_col(position = "dodge")

ggplot(filter(df, fylke!="norge"), aes(date, cumsum, fill=fylke)) +
  geom_col() + 
  geom_line(mapping=aes(date, cumsum), data=filter(df, fylke=="norge")) + 
  geom_point(mapping=aes(date, cumsum), data=filter(df, fylke=="norge"))
  


g = jsonlite::fromJSON("~/Downloads/Basisdata_0000_Norge_25833_Fylker_GEOJSON.geojson", FALSE)
f1 <- g$administrative_enheter.fylke$features[1]
f1[[1]]$geometry$coordinates[[1]][[1]]

df <- read_delim(
  "data/Personer1.utf8.csv", delim=";", 
  escape_double = TRUE, col_names = FALSE, skip=3
) %>%
  rename(county_raw=X1, sex=X2, population=X3) %>%
  mutate(county_code = word(county_raw, 1)) %>%
  mutate(county = word(county_raw, 2, -1)) %>%
  mutate(county = ifelse(grepl('Troms', county), word(county, 1, 3), county)) %>%
  mutate(county = ifelse(grepl('Trøndelag', county), word(county, 1), county))
df

write_csv(df, "data/populasjon.csv")

df %>% group_by(county, county_code) %>%
  summarise(population = sum(population))


df <- read_delim(
  "data/Folkemengde.utf8.csv", delim=";", 
  escape_double = TRUE, col_names = FALSE, skip=3
) %>%
  rename(municipality_raw=X1, population=X2) %>%
  mutate(municipality_code = word(municipality_raw, 1)) %>%
  mutate(municipality = word(municipality_raw, 2, -1)) 

df  
write_csv(df, "data/populasjon_kommuner.csv")

library(curl)
con <- curl_fetch_memory("https://www.vg.no/spesial/2020/corona-viruset/data/norway-table-overview/")
all_cases <- jsonlite::fromJSON(rawToChar(con$content))
all_cases



con <- curl("https://www.vg.no/spesial/2020/corona-viruset/data/norway-region-data/")
con
d <- curl_fetch_memory("https://www.vg.no/spesial/2020/corona-viruset/data/norway-table-overview/?region=county")
d$status_code
e <- jsonlite::fromJSON(rawToChar(d$content))
e

## fetch data ##
con_all <- curl_fetch_memory("https://www.vg.no/spesial/2020/corona-viruset/data/norway-allCases/")
all <- jsonlite::fromJSON(rawToChar(con_all$content))

all

for(i in 1:length(all)) {
  all[[i]]$date = dates[i]
}
all



dates <- names(all)
for(i in 1:length(all)) {
  all[[i]]$date = dates[i]
}
all

df <- as_tibble(bind_rows(all))
df <- df %>%
  mutate(municipality = str_remove(municipality, '"')) %>%
  mutate(date = ymd(date)) %>%
  mutate(date = ymd(confirmed))

df
write_csv(df, "./data/vg.csv")

df %>% drop_na(dead)
df %>% drop_na(recovered)

# time_series_county
df <- read_csv("./data/vg.csv")
df

by_date_region <- df %>%
  group_by(date, county) %>%
  tally() %>%
  rename(daily=n) %>%
  arrange(county, date)
by_date_region
table(by_date_region$county)

splt <- by_date_region %>%
  group_by(county) %>%
  group_split(county)
splt

splt <- lapply(splt, function(x) {
  x$cumsum = cumsum(x$daily)
  return(x)
})
splt

df_county <- bind_rows(splt)
table(df_county$county)

# time_series_totals
df <- read_csv("data/vg.csv")
by_date <- df %>%
  group_by(date) %>%
  tally() %>%
  rename(daily=n) %>%
  mutate(cumsum = cumsum(daily))
df_totals <- by_date


df_county %>% 
  select(-daily) %>% 
  arrange(desc(date)) %>%
  pivot_wider(
    names_from = c(county), 
    values_from = c(cumsum)
) %>%
  fill(-date)
  
df <- left_join(df_totals, df_county, by="date")    
df

by_region <- df %>%
  group_by(county) %>%
  tally() %>%
  rename(daily=n)
by_region

by_date_region <- df %>%
  group_by(date, county) %>%
  tally() %>%
  arrange(county, date)
by_date_region

splt <- by_date_region %>%
  group_by(county) %>%
  group_split(county)

df <- lapply(
  splt, function(x) {
    x$new = cumsum(x$n)
    return(x)
  }
)

df <- bind_rows(df)
df


df %>% filter(county == "Norland")
