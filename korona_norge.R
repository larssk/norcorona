
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
  rename(fylke=X1, sex=X2, population=X3) %>%
  mutate(fylke = word(fylke, 2, -1)) %>%
  group_by(fylke) %>%
  summarise(population=sum(population)) %>%
  mutate(fylke = ifelse(grepl('Troms', fylke), word(fylke, 1, 3), fylke)) %>%
  mutate(fylke = ifelse(grepl('Trøndelag', fylke), word(fylke, 1), fylke)) %>%
  mutate(short = tolower(fylke)) %>%
  mutate(short = word(short, 1))
df

write_csv(df, "data/populasjon.csv")

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
)
  
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
