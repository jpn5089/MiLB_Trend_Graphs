library(tidyverse)
library(lubridate)
library(RcppRoll)
library(shiny)
library(DT)

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

server <- function(input, output, session) {
  
  output$text <- renderText({
    
    x <- "Latest Data Scrape - 05/02/2019"
    
    })
  
  pitcher_ids <- reactive({
    
    milbPitchers <- read.csv("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/MinorLeaguePitchers.csv") %>% 
      select(1,2,3,26) %>% 
      mutate(playername = paste0(Name, " (", Team, " Age: ", Age, ")"))
    
  })
  
  raw_pitcher_data <- reactive({
    
    data <- readRDS("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/pitcher_logs_2019-05-02.rds") %>%
      mutate(Season = year(Date), 
             Date2 = paste0("2017-",month(Date),"-",day(Date))) %>% 
      separate(IP, into = c("Innings", "Outs")) %>% 
      mutate(IP_calc = case_when(Outs == 1 ~ 1/3,
                                 Outs == 2 ~ 2/3,
                                 TRUE ~ as.numeric(Outs))) %>% 
      mutate(IP = as.numeric(Innings) + as.numeric(IP_calc)) %>%
      left_join(., pitcher_ids() %>% select(player = Name, playername)) 
    
  })
  
  rolling_pitcher_data <- reactive({
    
    # averages and rolling means
    
    milb_avgs <- readRDS("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/totals_pitchers_2019-05-02.rds") %>% 
      select(player, milb_k_perc = K_perc, milb_bb_perc = BB_perc, milb_avg = AVG, milb_whip = WHIP, 
             milb_babip = BABIP, milb_lob_perc = LOB_perc, milb_fip = FIP) %>% 
      mutate(milb_avg = round(as.numeric(milb_avg),2),
             milb_babip = round(as.numeric(milb_babip),2),
             milb_fip = round(as.numeric(milb_fip),2),
             milb_lob_perc = round(as.numeric(milb_lob_perc),2),
             milb_bb_perc = round(as.numeric(milb_bb_perc),2),
             milb_k_perc = round(as.numeric(milb_k_perc),2),
             milb_whip = round(as.numeric(milb_whip),2)) %>% 
      left_join(., pitcher_ids() %>% select(player = Name, playername))
      
    
    #if a SP (GS not 0), maybe 10, otherwise 20?
    
    rolling = input$rolling_pitcher
    
    rolling_means <- raw_pitcher_data() %>% 
      mutate(Roll_K = round(as.numeric(roll_mean(x = K_perc, n = rolling, fill=NA, align="right")),2),
             Roll_BB = round(as.numeric(roll_mean(as.numeric(BB_perc), rolling, fill=NA,align="right")),2),
             Roll_AVG = round(as.numeric(roll_mean(as.numeric(AVG), rolling, fill=NA,align="right")),2),
             Roll_WHIP = round(as.numeric(roll_mean(as.numeric(WHIP), rolling, fill=NA,align="right")),2),
             Roll_BABIP = round(as.numeric(roll_mean(as.numeric(BABIP), rolling, fill=NA,align="right")),2),
             Roll_LOB = round(as.numeric(roll_mean(as.numeric(LOB_perc), rolling, fill=NA,align="right")),2),
             Roll_FIP = round(as.numeric(roll_mean(as.numeric(FIP), rolling, fill=NA,align="right")),3),
             Roll_GS = as.numeric(roll_sum(as.numeric(GS), rolling, fill = NA, align = "right")))
    
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
      mutate(K_perc_diff = round(Roll_K - milb_k_perc,2),
             BB_perc_diff = round(Roll_BB - milb_bb_perc,2),
             AVG_diff = round(Roll_AVG - milb_avg,2),
             WHIP_diff = round(Roll_WHIP - milb_whip,2),
             BABIP_diff = round(Roll_BABIP - milb_babip,2),
             LOB_perc_diff = round(Roll_LOB - milb_lob_perc,2),
             FIP_diff = round(Roll_FIP - milb_fip,2),
             player = as.character(player),
             Level = gsub("\\(", "", Level),
             Level = gsub("\\)", "", Level)) %>% 
      left_join(., pitcher_ids() %>% select(player = Name, Age)) %>% 
      select(Date, Team, Level, player, Age, everything()) %>% 
      filter(!(Team %in% c("OAX", "LEO", "MVA", "TIG", "LAG", "LAR", "TAB", "MTY", "CAM", "DUR", "SAL",
                           "TIJ", "YUC", "MEX") & Level == "(AAA)")) %>% 
      filter(Age < 25) %>%
      # filter(K_perc_diff > 0 & BB_perc_diff < 0) %>% 
      select(Last_Game = Date, Team, Level, Player = player, Age, Roll_GS, `Roll_K%` = Roll_K, `MiLB_K%` = milb_k_perc,
             `K%_diff` = K_perc_diff) %>% 
      # select(Last_Game = Date, Team, Level, Player = player, Age, `Roll_K%` = Roll_K, `MiLB_K%` = milb_k_perc,
      #        `K%_diff` = K_perc_diff, `Roll_BB%` = Roll_BB, `MiLB_BB%` = milb_bb_perc,`BB%_diff` = BB_perc_diff,
      #        Roll_FIP, MiLB_FIP = milb_fip, FIP_diff,
      #        Roll_AVG, MiLB_AVG = milb_avg, AVG_diff, Roll_BABIP, MiLB_BABIP = milb_babip, BABIP_diff,
      #        Roll_WHIP, MiLB_WHIP = milb_whip, WHIP_diff, 
      #        `Roll_LOB%` = Roll_LOB, `MiLB_LOB%` = milb_lob_perc,`LOB%_diff` = LOB_perc_diff) %>% 
      arrange(desc(`K%_diff`)) 
      
    
    list(milb_avgs = milb_avgs, comparison_to_career_avg = comparison_to_career_avg)
    
  })
  
  output$plot_pitcher <- renderPlot({
    
    footerComment <- "-----------------------------------------------------------------------------------------------------------------------------------
    Data from Fangraphs.com
    Complete credit goes to smadaplaysfantasy for the blueprint 
    via his app at https://www.prospectslive.com/minor-graphs, Twitter: @smada_bb"
    
    player_name = input$playername_pitcher
    
    titlevar <- "MiLB"
    pdata <- raw_pitcher_data() %>%  
      filter(playername == input$playername_pitcher) 
    
    rolling = input$rolling_pitcher
    
    rolling_means_graph <- pdata %>%  
      mutate(Roll_K = round(as.numeric(roll_mean(x = K_perc, n = rolling, fill=NA, align="right")),3),
             Roll_BB = round(as.numeric(roll_mean(as.numeric(BB_perc), rolling, fill=NA,align="right")),3),
             Roll_AVG = round(as.numeric(roll_mean(as.numeric(AVG), rolling, fill=NA,align="right")),3),
             Roll_WHIP = round(as.numeric(roll_mean(as.numeric(WHIP), rolling, fill=NA,align="right")),3),
             Roll_FIP = round(as.numeric(roll_mean(as.numeric(FIP), rolling, fill=NA,align="right")),3),
             Roll_BABIP = round(as.numeric(roll_mean(as.numeric(BABIP), rolling, fill=NA,align="right")),3),
             Roll_LOB = round(as.numeric(roll_mean(as.numeric(LOB_perc), rolling, fill=NA,align="right")),3)) %>% 
      filter(!is.na(Roll_K)) %>% 
      filter(year(Date) > 2015) 
    
    if (input$metric_pitcher == "K%") {
      
      ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_K)) + 
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_K, group=Level, color=Level),size=1.5) + 
        geom_point(aes(y=Roll_K, group=Level, color=Level), size=2.5, alpha = 1) +
        geom_hline(yintercept=rolling_pitcher_data()$milb_avgs$milb_k_perc[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],linetype="dashed", alpha=1) +
        facet_wrap(~Season) +
        ggtitle(paste(player_name,titlevar,"K% rolling",rolling,"game average"), 
                subtitle = paste0("Dashed Line = MiLB Career Average (", 
                                  round(rolling_pitcher_data()$milb_avgs$milb_k_perc[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],3)*100, "%)")) +
        labs(caption = footerComment) +
        labs(x="Month", y="K Rate") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
        # geom_point_interactive(aes(tooltip = paste(paste0("Last ",30," Games up to ", Date), 
        # Level, paste0(100*Roll_K,"% K Rate"), sep='\n'), 
        # data_id= Date, color=Level),size=1) +
        geom_smooth(se=F, linetype ="solid") +
        theme_smada()
      
    } else if (input$metric_pitcher == "BB%") {
      
      ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_BB)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_BB, group=Level, color=Level), size=1.5) + 
        geom_point(aes(y=Roll_BB, group=Level, color=Level), size=2.5, alpha=1) +
        geom_hline(yintercept=rolling_pitcher_data()$milb_avgs$milb_bb_perc[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],linetype="dashed", alpha=1) +
        facet_wrap(~Season) +
        ggtitle(paste(player_name,titlevar,"BB% rolling",rolling,"game average"), 
                subtitle = paste0("Dashed Line = MiLB Career Average (", 
                                  round(rolling_pitcher_data()$milb_avgs$milb_bb_perc[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],3)*100, "%)")) +
        labs(caption = footerComment) +
        labs(x="Month", y="BB Rate") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
        # geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
        # Level, paste0(100*Roll_BB, "% BB Rate"), sep='\n'), 
        # data_id= Date, color=Level), size=1) +
        geom_smooth(se=F,linetype="solid") +
        theme_smada()
      
    } else if (input$metric_pitcher == "FIP") {
      
      ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_FIP)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_FIP, group=Level, color=Level), size=1.5) + 
        geom_point(aes(y=Roll_FIP, group=Level, color=Level), size=2.5, alpha=1) +
        geom_hline(yintercept=rolling_pitcher_data()$milb_avgs$milb_fip[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],linetype="dashed", alpha=1) +
        facet_wrap(~Season) +
        ggtitle(paste(player_name,titlevar,"FIP rolling",rolling,"game average"), 
                subtitle = paste0("Dashed Line = MiLB Career Average (", 
                                  round(rolling_pitcher_data()$milb_avgs$milb_fip[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],3), ")")) +
        labs(caption = footerComment) +
        labs(x="Month", y="FIP") +
        scale_x_date(date_breaks = "1 month", date_labels="%b") +
        # scale_y_continuous(limits=c(0, NA), labels = scales::percent) +
        # geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
        # Level, paste0(100*Roll_BB, "% BB Rate"), sep='\n'), 
        # data_id= Date, color=Level), size=1) +
        geom_smooth(se=F,linetype="solid") +
        theme_smada()
    
    } else {
      
      ggplot(data=rolling_means_graph, aes(as.Date(Date2), Roll_AVG)) +
        # Line, Points, Smoothed Line
        geom_line(aes(y=Roll_AVG,group=Level, color=Level),size=1.5) + 
        ylim(0, NA) +
        geom_point(aes(y=Roll_AVG,group=Level, color=Level), size=2.5, alpha=1) +
        geom_smooth(se=F, linetype="solid") +
        geom_hline(yintercept=rolling_pitcher_data()$milb_avgs$milb_avg[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],linetype="dashed", alpha=1) + 
        facet_wrap(~Season) +
        ggtitle(paste(player_name,titlevar,"AVG rolling",rolling,"game average"), 
                subtitle = paste0("Dashed Line = MiLB Career Average (", 
                                  round(rolling_pitcher_data()$milb_avgs$milb_avg[which(rolling_pitcher_data()$milb_avgs$playername == player_name)],3), ")")) +
        labs(caption = footerComment) +
        labs(x="Month", y="AVG") +
        scale_x_date(date_breaks = "1 month", date_labels="%b")+
        # geom_point_interactive(aes(tooltip = paste(paste0("Last ",isolate(input$rolling)," Games up to ", Date), 
        # Level, paste0(Roll_BA," BA"),sep='\n'), 
        # data_id= Date, color=Level),size=1) +
        theme_smada()
      
    }
    
  })
  
  output$trends_pitcher <- renderDataTable({
      
     x <- rolling_pitcher_data()$comparison_to_career_avg 
    
    datatable(x,
              filter = 'top',
              rownames = FALSE) %>% 
      formatStyle("K%_diff", color = styleInterval(0, c("red", "green"))) 
    # %>% 
    #   formatStyle("BB%_diff", color = styleInterval(0, c("green", "red"))) %>% 
    #   formatStyle("LOB%_diff", color = styleInterval(0, c("red", "green"))) %>% 
    #   formatStyle("WHIP_diff", color = styleInterval(0, c("green", "red"))) %>% 
    #   formatStyle("BABIP_diff", color = styleInterval(0, c("red", "green"))) %>% 
    #   formatStyle("FIP_diff", color = styleInterval(0, c("green", "red"))) %>% 
    #   formatStyle("AVG_diff", color = styleInterval(0, c("green", "red")))
    
  })
  
}