library(tidyverse)
library(lubridate)
library(RcppRoll)

milbBatters <- read.csv("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/MinorLeagueBatters.csv") %>% 
  select(1,2,3,26) %>% 
  rename(name = `ï..Name`) %>% 
  mutate(playername = paste0(name, " (", Team, " Age: ", Age, ")"))

milb_batter_game_logs_fg <- function(playerid, year = 2017) {
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=PB","&type=-1")
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=PB","&type=-2")
  # standard table
  payload1 <- xml2::read_html(url_basic) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  
  payload1 <- payload1 %>%
    dplyr::filter(!grepl("Date", Date)) 
  # advanced table
  payload2 <- xml2::read_html(url_adv) %>%
    rvest::html_nodes("table") %>%
    .[length(.)] %>%
    rvest::html_table() %>%
    as.data.frame()
  payload2 <- payload2 %>%
    dplyr::filter(!grepl("Date", Date)) %>% 
    dplyr::rename(BB_perc = BB., K_perc = K.,
                  wRC_plus = wRC., BB_per_K = BB.K)
  payload2 <- as.data.frame(sapply(payload2, function(x) (gsub("\\ %", "", x))),
                            stringsAsFactors=F)
  
  payload2$BB_perc <- as.numeric(payload2$BB_perc)/100
  payload2$K_perc <- as.numeric(payload2$K_perc)/100
  # combine standard & advanced game log tabs
  payload <- merge(payload1,payload2) %>% 
    filter(Date != "Total")
  
  totals <- merge(payload1,payload2) %>% 
    filter(Date == "Total")
  
  # separate Team column into Team & MiLB level
  payload <- payload %>% 
    separate(Team, into = c("Team","Level"),sep=" ")
  
  list(payload = payload, totals = totals)
}  

milb_adv_scrape_game <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=batter&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% filter(!is.na(hc_x)) %>% 
    mutate(Date = game_date,
           GB = if_else(hit_trajectory=="ground_ball",1,0),
           FB = if_else(hit_trajectory=="fly_ball",1,0),
           LD = if_else(hit_trajectory=="line_drive",1,0),
           PU = if_else(hit_trajectory=="popup",1,0),
           a = sqrt(250^2+(-250)^2),
           b = sqrt(((hc_x)-(-125))^2+((250-hc_y)-295)^2),
           c = sqrt(((hc_x)-125)^2+((250-hc_y)-45)^2),
           angle = 180-acos((b^2-a^2-c^2)/(2*a*c))*180/pi,
           Pull = if_else(angle<30 & bat_side=="R", 1, if_else(angle>=60 & bat_side=="L",1,0)),
           Center = if_else(angle<60 & angle>=30, 1, 0),
           Oppo = if_else(angle>=60 & bat_side=="R", 1, if_else(angle<30 & bat_side=="L",1,0))
    )
  
  game_summary <- game_summary %>% group_by(Date) %>% 
    summarise(GB=sum(GB), FB=sum(FB), LD=sum(LD), PU=sum(PU), 
              Pull=sum(Pull), Center=sum(Center), Oppo=sum(Oppo)) 
  
  game_summary <- game_summary %>% group_by(Date) %>% 
    mutate(in_play = sum(GB,FB,LD,PU), FBPU = sum(FB,PU)) %>% 
    filter(in_play != 0) %>% 
    mutate(Season = year(Date),Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
    ungroup()
}

milb_adv_scrape_in_play <- function(playerid){
  
  vars1="pitcher_throws=&batter_stands=&game_date_gt=&game_date_lt=&home_away=&draft_year=&prospect=&player_type=batter&sort_by=results&sort_order=desc&group_by=name&min_results=&players="
  vars2="&min_pa=1#results"
  
  url <- paste0("https://www.mlb.com/prospects/stats/search/csv?", vars1, playerid,vars2)
  payload <- readr::read_csv(url, na = "null")
  game_summary <- payload %>% filter(!is.na(hc_x)) %>% 
    mutate(Date = game_date,
           GB = if_else(hit_trajectory=="ground_ball",1,0),
           FB = if_else(hit_trajectory=="fly_ball",1,0),
           LD = if_else(hit_trajectory=="line_drive",1,0),
           PU = if_else(hit_trajectory=="popup",1,0),
           a = sqrt(250^2+(-250)^2),
           b = sqrt(((hc_x)-(-125))^2+((250-hc_y)-295)^2),
           c = sqrt(((hc_x)-125)^2+((250-hc_y)-45)^2),
           angle = 180-acos((b^2-a^2-c^2)/(2*a*c))*180/pi,
           Pull = if_else(angle<30 & bat_side=="R", 1, if_else(angle>=60 & bat_side=="L",1,0)),
           Center = if_else(angle<60 & angle>=30, 1, 0),
           Oppo = if_else(angle>=60 & bat_side=="R", 1, if_else(angle<30 & bat_side=="L",1,0)),
           est_distance = 2.20*sqrt((250-hc_x-125)^2+(250-hc_y-45)^2)+13.1
    )
}

year <- "all"

battersMiLB <- read.csv("data/milb_2018_batter_ids.csv",header=T) %>%
  mutate(NameTeam = as.character(paste0(Name," (",Team,")"))) %>% 
  filter(playedid %in% milbBatters$playerid)

raw_batter_logs <- NULL
raw_savant_data <- NULL
raw_totals_logs <- NULL

for (i in 1:nrow(battersMiLB)) {
  
  data_scrape_game_logs <- milb_batter_game_logs_fg(battersMiLB$playerid[i],year)
  
  batter_logs <- data_scrape_game_logs$payload %>% 
    mutate(player = milbPitchers$name[i])
  
  totals_batter_logs <- data_scrape_game_logs$totals %>% 
    mutate(player = milbPitchers$name[i])
  
  batter_logs_savant <- milb_adv_scrape_game(battersMiLB$mlbid[i]) %>% 
    mutate(player = battersMiLB$Name[i])
  
  raw_savant_data[[i]] <- batter_logs_savant
  raw_batter_logs[[i]] <- batter_logs
  raw_totals_logs[[i]] <- totals
  
  print(paste0(i, ": ", battersMiLB$name[i]))
  
}

raw_savant <- do.call(rbind, raw_savant_data) 
batter_logs <- do.call(rbind, raw_batter_logs) 
stat_avgs <- do.call(rbind, raw_totals_logs) 
