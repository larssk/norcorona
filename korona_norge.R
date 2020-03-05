
library(tidyverse)
library(lubridate)
library(readr)

df <- read_csv("data/norge.csv")
df

df <- df %>% 
  mutate(cumsum = cumsum(daily))
df

ggplot(df, aes(date, cumsum), col=1) +
  geom_line() +
  geom_col(mapping = aes(date, daily), fill=2) +
  geom_point() + 
  xlab("Dato") + ylab("Smittede")

df <- read_csv("data/fylke.csv")
df

df2 <- pivot_longer(df, cols = -date, names_to = "fylke", values_to = "cumsum")
df2

df3 <- df2 %>%
  filter(fylke %in% c("vestland", "viken"))

ggplot(df3, aes(date, cumsum, col=fylke)) +
  geom_line()
