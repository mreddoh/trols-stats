
## LOAD PACKAGES ----
library(tidyverse)

## REQUIREMENTS ----
# Run /scrape_trols_data.R to create matchScores dataset.
if (!exists("matchScores")) { source("scrape_trols_data.R") }
source("elo_doubles().R")

## CALCULATE SUMMARY STATS ----
matchStats <- bind_rows((matchScores[,c(1,2,3,5,9,10)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost"))),
                        (matchScores[,c(1,2,3,6,9,10)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost"))),
                        (matchScores[,c(1,2,4,7,10,9)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost"))),
                        (matchScores[,c(1,2,4,8,10,9)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost")))) %>% 
  filter(!is.na(gamesWon)) %>% 
  mutate(winFlag = ifelse(gamesWon==6,1,0),
         gamesWon = as.integer(gamesWon),
         gamesLost = as.integer(gamesLost)) %>% 
  group_by(Team, Player) %>% 
  summarise(setsWon = sum(winFlag),
            setsLost = n() - sum(winFlag),
            percentageGames = 100*(sum(gamesWon) / sum(gamesLost)),
            percentageSets = 100*(sum(setsWon) / sum(setsLost)))

## CALCULATE ELO RATINGS ----

