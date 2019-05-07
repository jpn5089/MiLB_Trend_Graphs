library(shiny)
library(DT)
library(shinycssloaders)
library(dplyr)

battersMiLB <- read.csv("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/milb_2018_batter_ids.csv",header=T) %>%
  mutate(NameTeam = paste0(Name, " (", Team, " Age: ", Age, ")"))

pitchersMiLB <- read.csv("C://Users/johnp/Documents/GitHub/MiLB_Trend_Graphs/data/MinorLeaguePitchers.csv") %>% 
  select(1,2,3,26) %>% 
  mutate(playername = paste0(Name, " (", Team, " Age: ", Age, ")"))

ui <- navbarPage(
  "Minor League Reports",
  tabPanel("Pitchers Report",
           fluidPage(
             textOutput("text"),
             tags$head(tags$style("#text{color: red;
                                 font-size: 17px;
                                 font-style: italic;
                                 }"
             )
             ),
             
             
             br(),
             
             DT::dataTableOutput("trends_pitcher") %>% withSpinner(type = 8)
             )
           
  ),
  tabPanel("Pitchers Chart",
           fluidPage(
             fluidRow(
               column(3, 
                      selectInput("playername_pitcher", "Player (delete & type)", choices = pitchersMiLB$playername)
                      ),
               column(2,
                      selectInput("metric_pitcher", "Metric", 
                            choices = list("K%", "BB%", "AVG", "WHIP", "BABIP", "LOB%", "FIP"
                            ))
                      ),
               column(3,
                      sliderInput("rolling_pitcher",
                            "Games For Rolling Avg (Used for trends table also): ",
                            min = 1, max=162, value = 15)
                      )
               # column(2,
               #        actionButton("plot", "Draw Plot")
               # )
               ),
             mainPanel(
               plotOutput("plot_pitcher", width = "1200px", height = "600px") %>% withSpinner(type = 8)
               
             )
           )
  )
  # tabPanel("Pitchers",
  #          fluidPage(
  #            fluidRow(
  #              column(4,
  #                     selectInput("playername_pitcher", "Player (delete & type)", choices = pitchersMiLB$playername, 
  #                                 selected = "Shane Bieber (- - - Age: 23)"),
  #                     # Choose Metric
  #                     selectInput("metric_pitcher", "Metric", 
  #                                 choices = list("K%", "BB%", "AVG", "WHIP", "BABIP", "LOB%", "FIP"
  #                     )),
  #                     # slider for number of games for rolling avg
  #                     sliderInput("rolling_pitcher",
  #                                 "Games For Rolling Avg (Used for trends table also): ",
  #                                 min = 5, max=162, value = 15),
  #                     actionButton("plot", "Draw Plot")
  #              ),
  #              column(7, offset = 1,
  #                     plotOutput("plot_pitcher") %>% withSpinner(type = 8)
  #              )
  #            ),
  #            br(),
  #            br(),
  #            fluidRow(
  #              DT::dataTableOutput("trends_pitcher") %>% withSpinner(type = 8)
  #            )
  #          )
  # )
  # ,
  # tabPanel("Batters",
  #          fluidPage(
  #            fluidRow(
  #              column(4,
  #                     selectInput("playername_batter", "Player (delete & type)", choices = battersMiLB$NameTeam, selected = "Eloy Jimenez (- - -)"),
  #                     # Choose Metric
  #                     selectInput("metric_batter", "Metric", choices = list(
  #                       "wRC+", "ISO", "K%", "BB%", "BABIP", "BA", "OBP", "SLG", "SB & Attempts per 600 PA", 
  #                       "FB%", "GB%", "LD%", "PU%", "HR/FB", "Estimated FB Distance","Pull%", "Cent%", "Oppo%" 
  #                     )),
  #                     # slider for number of games for rolling avg
  #                     sliderInput("rolling_batter",
  #                                 "Games For Rolling Avg:",
  #                                 min = 5, max=162, value = 30)
  #              ),
  #              column(7, offset = 1,
  #                     plotOutput("plot_batter")
  #              )
  #            ),
  #            br(),
  #            br(),
  #            fluidRow(
  #              DT::dataTableOutput("trends_batter")
  #            )
  #          )
  # )
)
  
  