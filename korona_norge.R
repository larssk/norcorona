
library(tidyverse)
library(lubridate)
library(readr)

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
  


