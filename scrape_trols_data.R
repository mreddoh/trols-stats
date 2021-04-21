
## LOAD PACKAGES ----
library(tidyverse)
library(lubridate)
library(rvest)

## CREATE MATCH IDS ----
sectionID = "TN014"

updateTime <- read_html(paste0("https://trols.org.au/nentg/results.php?daytime=",substr(sectionID,1,2))) %>% 
  html_elements('span') %>%
  .[1] %>% 
  as.character() %>% 
  gsub("<.*?>", "", .) %>% 
  str_extract(.,"\\S{2,}\\s\\w{3,}\\s\\d{2}") %>% 
  dmy()
  
webpageFixture <- read_html(paste0("https://trols.org.au/nentg/fixture.php?style=1&daytime=",substr(sectionID,1,2),"&section=",sectionID,"&team=",substr(sectionID,1,2),"001&style=1"))

roundID = webpageFixture %>% 
  html_nodes("tr") %>%
  html_text() %>% 
  as.data.frame() %>% 
  filter(str_detect(.,"^([0-9]+)\\s([0-9]+)")) %>% 
  mutate(Round = as.integer(str_extract(.,"^([0-9]{1,2})")),
         Date = as.Date(str_extract(.,"[0-9]{1,2}\\s\\D{3}\\s[0-9]{2}"), "%d %b %y")) %>% 
  filter(Date < updateTime) %>% 
  pull(Round)

orderID = c(1:4)

matchIDlist <- crossing(sectionID,roundID,orderID) %>% 
  mutate(matchID = paste0(sectionID,str_pad(roundID,2,"left","0"),orderID)) %>% 
  pull(matchID)

## LOOP FOR SCRAPING ----
for (m in 1:length(matchIDlist)){

##* SCRAPE WEBSITE ----
matchID = matchIDlist[m]
webpage <- read_html(paste0("https://trols.org.au/nentg/match_popup.php?matchid=",matchID))

##* WRANGLE DATA ----
homeTeam <- webpage %>% html_elements("b") %>% .[1] %>% as.character() %>% gsub("<.*?>", "", .)
awayTeam <- webpage %>% html_elements("b") %>% .[2] %>% as.character() %>% gsub("<.*?>", "", .)

byeFlag <- ifelse(toupper(homeTeam)=="BYE"|toupper(awayTeam)=="BYE",1,0)

  if(byeFlag == 0) {
    
    matchDt <- webpage %>% 
      html_table() %>% 
      .[[1]] %>% 
      as.data.frame() %>% 
      mutate(Date = dmy(str_extract(X1,"\\S+\\s\\S+\\s\\S+$"))) %>% 
      pull(Date)
    
    tempHome <- webpage %>% 
      html_table() %>% 
      .[[3]] %>% 
      as.data.frame() %>% 
      mutate(Team = homeTeam,
             Position = str_extract(X2,"^."),
             Player = trimws(str_extract(X2,"[^.]+$")),
             Emergency = ifelse(X1 == "E",1,0)) %>% 
      select(Team,Position,Player,Emergency)
    
    tempAway <- webpage %>% 
      html_table() %>% 
      .[[5]] %>% 
      as.data.frame() %>% 
      mutate(Team = awayTeam,
             Position = str_extract(X2,"^."),
             Player = trimws(str_extract(X2,"[^.]+$")),
             Emergency = ifelse(X1 == "E",1,0)) %>% 
      select(Team,Position,Player,Emergency)
    
    matchScores_temp <- webpage %>% 
      html_table() %>% 
      .[[4]] %>% 
      as.data.frame() %>% 
      filter(row_number() != max(row_number())) %>% 
      mutate(setID = row_number(),
             Score = X2,
             homeScore = str_extract(X2,"^."),
             awayScore = str_extract(X2,".$"),
             P1 = str_extract(X1,"^."),
             P2 = str_extract(X1,".$"),
             posPlaying = X1) %>% 
      left_join(.,tempHome,by=c("P1"="Position")) %>% 
      left_join(.,tempHome,by=c("P2"="Position")) %>% 
      rename(homeP1 = Player.x, 
             homeP2 = Player.y) %>% 
      left_join(.,tempAway,by=c("P1"="Position")) %>% 
      left_join(.,tempAway,by=c("P2"="Position")) %>% 
      rename(awayP1 = Player.x, 
             awayP2 = Player.y) %>% 
      mutate(Date = matchDt,
             homeTeam = homeTeam,
             awayTeam = awayTeam) %>% 
      select(Date,setID,homeTeam,awayTeam,homeP1,homeP2,awayP1,awayP2,homeScore,awayScore)
    
    if(m == 1) { matchScores <- matchScores_temp }
    if(m > 1) { matchScores <- rbind(matchScores,matchScores_temp) }  
    
    rm(matchScores_temp)
    
  }

}





