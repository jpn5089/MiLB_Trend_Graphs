library(tidyverse)
library(lubridate)
library(RcppRoll)

# To grab minor league ids
# https://www.fangraphs.com/minorleaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=0&season=2018&team=0&players=

#regrab player ids with lower IP threshold

milbPitchers <- read.csv("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/rolling_average_report/data/MinorLeaguePitchers.csv") %>% 
  select(1,2,3,26) %>% 
  mutate(playername = paste0(Name, " (", Team, " Age: ", Age, ")")) %>% 
  filter(!grepl("Mexican", Team)) 

milb_pitcher_game_logs_fg <- function(playerid, year = 2017) {
  # url for standard game log table
  url_basic <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                      playerid,
                      "&season=",
                      year,
                      "&position=P","&type=-1")
  # url for advanced game log table
  url_adv <- paste0("http://www.fangraphs.com/statsd.aspx?playerid=",
                    playerid,
                    "&season=",
                    year,
                    "&position=P","&type=-2")
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
                  K_per_9 = K.9, BB_per_9 = BB.9,
                  K_per_BB = K.BB, HR_per_9 = HR.9, 
                  K_perc_minus_BB_perc = K.BB.,
                  LOB_perc = LOB.)
  payload2 <- as.data.frame(sapply(payload2, function(x) (gsub("\\ %", "", x))),
                            stringsAsFactors=F)
  
  payload2$BB_perc <- as.numeric(payload2$BB_perc)/100
  payload2$K_perc <- as.numeric(payload2$K_perc)/100
  payload2$LOB_perc <- as.numeric(payload2$LOB_perc)/100
  payload2$K_perc_minus_BB_perc <- as.numeric(payload2$K_perc_minus_BB_perc)
  # combine standard & advanced game log tabs
  payload <- merge(payload1,payload2) %>% 
    filter(Date != "Total")
  
  last_team <- payload %>% 
    filter(Date == max(Date)) %>% 
    pull(Team)
  
  totals <- merge(payload1,payload2) %>% 
    filter(Date == "Total") %>% 
    mutate(Team = last_team)
  
  # separate Team column into Team & MiLB level
  payload <- payload %>% 
    separate(Team, into = c("Team","Level"),sep=" ")
  
  list(payload = payload, totals = totals)
} 

year <- "all"

raw_pitcher_logs <- list()
raw_totals_logs <- list()

for (i in 120:nrow(milbPitchers)) {

  data_scrape <- milb_pitcher_game_logs_fg(milbPitchers$PlayerId[i],year)

  pitcher_logs <- data_scrape$payload %>%
    mutate(player = milbPitchers$Name[i],
           playerid = milbPitchers$PlayerId[i])

  totals <- data_scrape$totals %>%
    mutate(player = milbPitchers$Name[i],
           playerid = milbPitchers$PlayerId[i])

  raw_pitcher_logs[[i]] <- pitcher_logs
  raw_totals_logs[[i]] <- totals

  print(paste0(i, ": ", milbPitchers$Name[i]))

}

raw_pitcher_data <- do.call(rbind, raw_pitcher_logs)

totals <- do.call(rbind, raw_totals_logs)

saveRDS(raw_pitcher_data, paste0("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/rolling_average_report/data/pitcher_logs_", today(), ".rds"))

saveRDS(totals, paste0("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/rolling_average_report/data/totals_pitchers_", today(), ".rds"))


# quantitative analysis ---------------------------------------------------

raw_pitcher_data <- readRDS(paste0("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/rolling_average_report/data/pitcher_logs_", today(), ".rds")) %>%
  mutate(Season = year(Date), 
         Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
  separate(IP, into = c("Innings", "Outs")) %>% 
  mutate(IP_calc = case_when(Outs == 1 ~ 1/3,
                             Outs == 2 ~ 2/3,
                             TRUE ~ as.numeric(Outs))) %>% 
  mutate(IP = as.numeric(Innings) + as.numeric(IP_calc))

# averages and rolling means

milb_avgs <- readRDS(paste0("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/rolling_average_report/data/totals_pitchers_", today(), ".rds")) %>% 
  select(player, Team, milb_k_perc = K_perc, milb_bb_perc = BB_perc, milb_avg = AVG, milb_whip = WHIP, 
         milb_babip = BABIP, milb_lob_perc = LOB_perc, milb_fip = FIP) %>% 
  mutate(milb_avg = as.numeric(milb_avg),
         milb_babip = as.numeric(milb_babip),
         milb_fip = as.numeric(milb_fip),
         milb_whip = as.numeric(milb_whip))

#if a SP (GS not 0), maybe 10, otherwise 20?

rolling = 15

rolling_means <- raw_pitcher_data %>% 
  mutate(Roll_K = round(as.numeric(roll_mean(x = K_perc, n = rolling, fill=NA, align="right")),3),
         Roll_BB = round(as.numeric(roll_mean(as.numeric(BB_perc), rolling, fill=NA,align="right")),3),
         Roll_AVG = round(as.numeric(roll_mean(as.numeric(AVG), rolling, fill=NA,align="right")),3),
         Roll_WHIP = round(as.numeric(roll_mean(as.numeric(WHIP), rolling, fill=NA,align="right")),3),
         Roll_BABIP = round(as.numeric(roll_mean(as.numeric(BABIP), rolling, fill=NA,align="right")),3),
         Roll_LOB = round(as.numeric(roll_mean(as.numeric(LOB_perc), rolling, fill=NA,align="right")),3),
         Roll_FIP = round(as.numeric(roll_mean(as.numeric(FIP), rolling, fill=NA,align="right")),3),
         Roll_GS = as.numeric(roll_sum(as.numeric(GS), rolling, fill = NA, align = "right"))) %>% 
  filter(year(Date) > 2015) 

latest_rolling_value <- rolling_means %>% 
  mutate(Date = as_date(Date)) %>% 
  arrange(Date) %>% 
  group_by(player) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  filter(year(Date) > 2017) %>% 
  filter(!player %in% c("David Richardson", "Jose Oyervides", "Maikel Cleto", "Rafael Martin","Jorge Ibarra"))

options(scipen = 99999)

comparison_to_career_avg <- latest_rolling_value %>% 
  select(Date, Team, Level, player, contains("Roll")) %>% 
  left_join(., milb_avgs) %>% 
  mutate(K_perc_diff = Roll_K - milb_k_perc,
         BB_perc_diff = Roll_BB - milb_bb_perc,
         AVG_diff = Roll_AVG - milb_avg,
         WHIP_diff = Roll_WHIP - milb_whip,
         BABIP_diff = Roll_BABIP - milb_babip,
         LOB_perc_diff = Roll_LOB - milb_lob_perc,
         FIP_diff = Roll_FIP - milb_fip) %>% 
  left_join(., milbPitchers %>% select(player = Name, Age)) %>% 
  select(Date, Team, Level, player, Age, everything()) %>% 
  filter(!(Team %in% c("OAX", "LEO", "MVA", "TIG", "LAG", "LAR", "TAB", "MTY", "CAM", "DUR", "SAL",
                       "TIJ", "YUC") & Level == "(AAA)")) %>% 
  filter(Age < 25) %>% 
  filter(K_perc_diff > 0 & BB_perc_diff < 0)

# data prep ---------------------------------------------------------------

theme_smada <- function(){
  theme(text = element_text("sans-serif"),
        plot.title = element_text(size=20, vjust=5),
        plot.subtitle = element_text(face="italic"),
        plot.caption = element_text(color="#696969", hjust=0),
        plot.margin = unit(c(1,1,1,1), "cm"),
        
        axis.title.x = element_text(size = 15, margin = margin(20, 20, 20, 20)),
        axis.title.y = element_text(size = 15, margin = margin(20, 20, 20, 20)), 
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        
        strip.text.x = element_text(size=12, face="bold"),
        
        legend.title = element_text(size=15, vjust = 7, face = "bold"),
        legend.text = element_text(size=12),
        legend.key.size = unit(1,"cm"),
        legend.box.background = element_rect(),
        legend.box.spacing = unit(1.5,"cm"),
        legend.box.margin = margin(20, 20, 20, 20),
        legend.justification = "top"
  )
}

titlevar <- "MiLB"

player_search = "Jordan Guerrero (- - - Age: 21)"
player_name = "Seth Elledge"

raw_pitcher_data <- readRDS("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/pitcher_logs.rds") %>%
  mutate(Season = year(Date), 
         Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
  separate(IP, into = c("Innings", "Outs")) %>% 
  mutate(IP_calc = case_when(Outs == 1 ~ 1/3,
                             Outs == 2 ~ 2/3,
                             TRUE ~ as.numeric(Outs))) %>% 
  mutate(IP = as.numeric(Innings) + as.numeric(IP_calc)) %>%
  left_join(., milbPitchers %>% select(player = name, playername)) %>% 
  filter(player == player_name) 

# %>% 
  # select(Date, Team, Level, K_per_9, BB_per_9, HR_per_9, K_perc, BB_perc, K_perc_minus_BB_perc, AVG,
         # WHIP, BABIP, LOB_perc = LOB., FIP, player)

# milb_babip_avgs <- raw_pitcher_data %>% 
#   group_by(player) %>% 
#   summarise(milb_babip = sum(as.numeric(H) - as.numeric(HR), na.rm = T)/sum(as.numeric(TBF) - as.numeric(HR) - as.numeric(BB) - as.numeric(HBP) - as.numeric(SO), na.rm = T)) %>% 
#   ungroup()
# 
# milb_fip_avgs <- raw_pitcher_data %>% 
#   group_by(player) %>% 
#   summarise(milb_fip = mean(as.numeric(FIP))) %>% 
#   ungroup()

rolling = 15

rolling_means_graph <- raw_pitcher_data %>% 
  mutate(Roll_K = round(as.numeric(roll_mean(x = K_perc, n = rolling, fill=NA, align="right")),3),
         Roll_BB = round(as.numeric(roll_mean(as.numeric(BB_perc), rolling, fill=NA,align="right")),3),
         Roll_AVG = round(as.numeric(roll_mean(as.numeric(AVG), rolling, fill=NA,align="right")),3),
         Roll_WHIP = round(as.numeric(roll_mean(as.numeric(WHIP), rolling, fill=NA,align="right")),3),
         Roll_FIP = round(as.numeric(roll_mean(as.numeric(FIP), rolling, fill=NA,align="right")),3),
         Roll_BABIP = round(as.numeric(roll_mean(as.numeric(BABIP), rolling, fill=NA,align="right")),3),
         Roll_LOB = round(as.numeric(roll_mean(as.numeric(LOB_perc), rolling, fill=NA,align="right")),3)) %>% 
  filter(!is.na(Roll_K))

# plots -------------------------------------------------------------------

footerComment <- "-----------------------------------------------------------------------------------------------------------------------------------
Data from Fangraphs.com
Complete credit goes to smadaplaysfantasy for the blueprint 
via his app at SmadaPlaysFantasy.com/MiLB_Trend_Graphs, Twitter: @smada_bb"

## K% graph
ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_K)) + 
  # Line, Points, Smoothed Line
  geom_line(aes(y=Roll_K, group=Level, color=Level),size=1.5) + 
  geom_point(aes(y=Roll_K, group=Level, color=Level), size=2, alpha = 1) +
  geom_hline(yintercept=milb_avgs$milb_k_perc[which(milb_avgs$player == player_name)],linetype="dashed", alpha=1) +
  facet_wrap(~Season) +
  ggtitle(paste(unique(raw_pitcher_data$playername),titlevar,"K% rolling",rolling,"game average"), 
          subtitle = paste0("Dashed Line = MiLB Career Average (", 
                            round(milb_avgs$milb_k_perc[which(milb_avgs$player == player_name)],3)*100, "%)")) +
  labs(caption = footerComment) +
  labs(x="Month", y="K Rate") +
  scale_x_date(date_breaks = "1 month", date_labels="%b") +
  scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
  # geom_point_interactive(aes(tooltip = paste(paste0("Last ",30," Games up to ", Date), 
  # Level, paste0(100*Roll_K,"% K Rate"), sep='\n'), 
  # data_id= Date, color=Level),size=1) +
  geom_smooth(se=F, linetype ="solid") +
  theme_smada()

### BB% graph
ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_BB)) +
  # Line, Points, Smoothed Line
  geom_line(aes(y=Roll_BB, group=Level, color=Level), size=1.5) + 
  geom_point(aes(y=Roll_BB, group=Level, color=Level), size=2, alpha=1) +
  geom_hline(yintercept=milb_avgs$milb_bb_perc[which(milb_avgs$player == player_name)],linetype="dashed", alpha=1) +
  facet_wrap(~Season) +
  ggtitle(paste(unique(raw_pitcher_data$playername),titlevar,"BB% rolling",rolling,"game average"), 
          subtitle = paste0("Dashed Line = MiLB Career Average (", 
                            round(milb_avgs$milb_bb_perc[which(milb_avgs$player == player_name)],3)*100, "%)")) +
  labs(caption = footerComment) +
  labs(x="Month", y="BB Rate") +
  scale_x_date(date_breaks = "1 month", date_labels="%b") +
  scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
  # geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                             # Level, paste0(100*Roll_BB, "% BB Rate"), sep='\n'), 
                             # data_id= Date, color=Level), size=1) +
  geom_smooth(se=F,linetype="solid") +
  theme_smada()

### FIP graph
ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_FIP)) +
  # Line, Points, Smoothed Line
  geom_line(aes(y=Roll_FIP, group=Level, color=Level), size=1.5) + 
  geom_point(aes(y=Roll_FIP, group=Level, color=Level), size=2, alpha=1) +
  geom_hline(yintercept=milb_avgs$milb_fip[which(milb_avgs$player == player_name)],linetype="dashed", alpha=1) +
  facet_wrap(~Season) +
  ggtitle(paste(unique(raw_pitcher_data$playername),titlevar,"FIP rolling",rolling,"game average"), 
          subtitle = paste0("Dashed Line = MiLB Career Average (", 
                            round(milb_avgs$milb_fip[which(milb_avgs$player == player_name)],3), ")")) +
  labs(caption = footerComment) +
  labs(x="Month", y="FIP") +
  scale_x_date(date_breaks = "1 month", date_labels="%b") +
  # scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
  # geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
  # Level, paste0(100*Roll_BB, "% BB Rate"), sep='\n'), 
  # data_id= Date, color=Level), size=1) +
  geom_smooth(se=F,linetype="solid") +
  theme_smada()

## AVG Graph
ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_AVG)) +
  # Line, Points, Smoothed Line
  geom_line(aes(y=Roll_AVG,group=Level, color=Level),size=1.5) + 
  ylim(0, NA) +
  geom_point(aes(y=Roll_AVG,group=Level, color=Level), size=2, alpha=1) +
  geom_smooth(se=F, linetype="solid") +
  geom_hline(yintercept=milb_avg_avgs$milb_avg, linetype="dashed", alpha=1) + 
  facet_wrap(~Season) +
  ggtitle(paste(player_search,titlevar,"AVG rolling",rolling,"game average"), 
          subtitle = paste0("Dashed Line = MiLB Career Average (", round(milb_avgs$milb_avg,3), ")")) +
  # labs(caption = footerComment) +
  labs(x="Month", y="AVG") +
  scale_x_date(date_breaks = "1 month", date_labels="%b")+
  # geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
                                             # Level, paste0(Roll_BA," BA"),sep='\n'), 
                             # data_id= Date, color=Level),size=1) +
  theme_smada()
