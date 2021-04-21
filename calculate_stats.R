
## LOAD PACKAGES ----
library(tidyverse)
library(elo)

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
k = 30
matchScores_ELO <- matchScores %>% 
  filter(!is.na(homeScore)) %>% 
  arrange(Date,setID) %>% 
  mutate(rdr = row_number(), h1 = NA, h2 = NA, a1 = NA, a2 = NA)

for (m in 1:nrow(matchScores_ELO)){

  playerScores_ELO <- bind_rows((matchScores_ELO[,c(1,11,3,5,9,10,12)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating"))),
                                (matchScores_ELO[,c(1,11,3,6,9,10,13)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating"))),
                                (matchScores_ELO[,c(1,11,4,7,10,9,14)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating"))),
                                (matchScores_ELO[,c(1,11,4,8,10,9,15)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating")))) %>% 
    filter(!is.na(gamesWon)) %>% 
    mutate(winFlag = ifelse(gamesWon==6,1,0),
           gamesWon = as.integer(gamesWon),
           gamesLost = as.integer(gamesLost))
  
  eloMatch <- matchScores_ELO[m,]
  
  h1 = playerScores_ELO %>% filter(setID < eloMatch$rdr & Player == eloMatch$homeP1) %>% filter(setID == max(setID)) %>% pull(Rating)
  h2 = playerScores_ELO %>% filter(setID < eloMatch$rdr & Player == eloMatch$homeP2) %>% filter(setID == max(setID)) %>% pull(Rating)
  a1 = playerScores_ELO %>% filter(setID < eloMatch$rdr & Player == eloMatch$awayP1) %>% filter(setID == max(setID)) %>% pull(Rating)
  a2 = playerScores_ELO %>% filter(setID < eloMatch$rdr & Player == eloMatch$awayP2) %>% filter(setID == max(setID)) %>% pull(Rating)
  
  if(!length(h1)) { h1 = 1500 }
  if(!length(h2)) { h2 = 1500 }
  if(!length(a1)) { a1 = 1500 }
  if(!length(a2)) { a2 = 1500 }
  
  H_P = (1 / (1 + 10^((mean(h1,h2)-mean(a1,a2)) / 400)))
  A_P = (1 / (1 + 10^((mean(a1,a2)-mean(h1,h2)) / 400)))
  
  homeWin <- eloMatch %>% mutate(homeWin = ifelse(homeScore==6,1,0)) %>% pull(homeWin)
  
  matchScores_ELO[m,12] = h1 + k*(homeWin - H_P)
  matchScores_ELO[m,13] = h2 + k*(homeWin - H_P)
  matchScores_ELO[m,14] = a1 + k*(1-homeWin - A_P)
  matchScores_ELO[m,15] = a2 + k*(1-homeWin - A_P)

}

playerScores_ELO <- bind_rows((matchScores_ELO[,c(1,11,3,5,9,10,12)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating"))),
                              (matchScores_ELO[,c(1,11,3,6,9,10,13)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating"))),
                              (matchScores_ELO[,c(1,11,4,7,10,9,14)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating"))),
                              (matchScores_ELO[,c(1,11,4,8,10,9,15)] %>% set_names(.,c("Date","setID","Team","Player","gamesWon","gamesLost","Rating")))) %>% 
  filter(!is.na(gamesWon)) %>% 
  mutate(winFlag = ifelse(gamesWon==6,1,0),
         gamesWon = as.integer(gamesWon),
         gamesLost = as.integer(gamesLost))


latestELO <- playerScores_ELO %>% 
  group_by(Player) %>% 
  arrange(desc(setID)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Team,Player,Rating) %>% 
  arrange(-Rating)



