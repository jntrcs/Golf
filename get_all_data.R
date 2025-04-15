#This script preps all data and loads it into a df called "all_data"
library(tidyverse)

all_data = read_csv("2024PGATourResults.csv") %>%
  filter(tournament!="Barracuda Championship" #stores data weird
         ) %>% 
  mutate(year=2024) %>%
  bind_rows(
    read_csv("2025PGATourResults_through_march27.csv") %>%
      mutate(year=2025) %>%
      rename(player=playerName)
  ) %>%
  bind_rows(
    read_csv("2025PGATourResultsLast2.csv") %>%
      mutate(year=2025) %>%
      rename(player=playerName)
  ) %>%
  mutate(tournament_year = paste(tournament, year)) %>%
  filter(round <= 4)#filter tie breakers