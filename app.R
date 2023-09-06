library(nflreadr)
library(ggplot2)
library(ggimage)
library(tidyverse)
library(ggrepel)
library(dplyr)
library(paletteer)
library(webshot)
library(ggthemes)
library(readr)
library(ggtext)
library(ggforce)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(na.tools)
library(stringr)
library(magick)
library(ggbeeswarm)
library(vip)
library(gtExtras)
library(nflfastR)
library(ThemePark)
library(knitr)
library(nflplotR)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(bslib)
library(reactable)
library(reactablefmtr)
library(shinyWidgets)

first_td_off_all <- read_csv("offensefirstTD.csv")
first_td_def_all <- read_csv("defensefirstTD.csv")
playersjoin <- read_csv("playerleaders.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

  theme = shinytheme("flatly"),
    # Application title
    titlePanel("NFL First Touchdown Scorer Tracker"),

    mainPanel(
      navbarPage("By: Arjun Menon",
                 tabPanel("Offense",
                          fluidRow(
                            column(6, align = 'center',
                                   selectInput("sznchoose1", "Season", c(2019, 2020, 2021, 2022, 2023), selected = 2022))),
                          mainPanel(
                            reactableOutput("offensetbl"), width = 12
                          )
                          ),
                 tabPanel("Defense",
                          fluidRow(
                            column(6, align = 'center',
                                   selectInput("sznchoose2", "Season", c(2019, 2020, 2021, 2022, 2023), selected = 2022))),
                          mainPanel(
                            reactableOutput("defensetbl"), width = 12
                          )
                 ),
                 tabPanel("Player Leaders",
                          fluidRow(
                            column(6, align = 'center',
                                   selectInput("sznchoose3", "Season", c(2019, 2020, 2021, 2022, 2023), selected = 2022)),
                            column(6, align = 'center',
                                   pickerInput("teaminput","Team:", choices=c("ARI", 'ATL', 'BAL', 'BUF',
                                                                               'CAR', 'CHI', 'CIN', 'CLE', 
                                                                               'DAL', 'DEN', 'DET', 'GB',
                                                                               'HOU', 'IND', 'JAX', 'KC',
                                                                               'LV', 'LAC', 'LA', 'MIA',
                                                                               'MIN', 'NE', 'NO', 'NYG',
                                                                               'NYJ', 'PHI', 'PIT', 'SF',
                                                                               'SEA', 'TB', 'TEN', 'WAS'), 
                                               options = list(`actions-box` = TRUE),multiple = T,
                                               selected = c("ARI", 'ATL', 'BAL', 'BUF',
                                                            'CAR', 'CHI', 'CIN', 'CLE', 
                                                            'DAL', 'DEN', 'DET', 'GB',
                                                            'HOU', 'IND', 'JAX', 'KC',
                                                            'LV', 'LAC', 'LA', 'MIA',
                                                            'MIN', 'NE', 'NO', 'NYG',
                                                            'NYJ', 'PHI', 'PIT', 'SF',
                                                            'SEA', 'TB', 'TEN', 'WAS')))),
                          mainPanel(
                            reactableOutput("playertbl"), width = 12
                          )
                 ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$offensetbl <- renderReactable({
    
    first_td_off_all <- first_td_off_all |> 
      filter(season == input$sznchoose1) |> 
      select(-season)
    
    reactable(first_td_off_all,
              compact = FALSE,
              pagination = FALSE,
              columns = list(
                first_td_team = colDef(name = "Offense",
                                       maxWidth = 80,
                                       align = "left", sticky = 'left'),
                team_logo_espn = colDef(name = "",
                                        maxWidth = 35,
                                        cell = embed_img(),
                                        align = "center", sticky = 'left'),
                first_td_times = colDef(name = "Total 1st TD", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                first_td_rush = colDef(name = "1st TD Rush", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                first_td_pass = colDef(name = "1st TD Pass", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                QB = colDef(name = "QB", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                RB = colDef(name = "RB", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                WR = colDef(name = "WR", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                TE = colDef(name = "TE", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                DEF = colDef(name = "DEF", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_off_all, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                first_td_perc = colDef(name = "% of Games", maxWidth = 75, align = 'center', format = colFormat(percent = TRUE, digits = 1))
                
              ), fullWidth = TRUE)
    
  })
  
  output$defensetbl <- renderReactable({
    
    first_td_def_all <- first_td_def_all |> 
      filter(season == input$sznchoose2) |> 
      select(-season)
    
    reactable(first_td_def_all,
              compact = FALSE,
              pagination = FALSE,
              columns = list(
                first_td_team_all = colDef(name = "Defense",
                                       maxWidth = 80,
                                       align = "left", sticky = 'left'),
                team_logo_espn = colDef(name = "",
                                        maxWidth = 35,
                                        cell = embed_img(),
                                        align = "center", sticky = 'left'),
                first_td_times = colDef(name = "Total 1st TD", maxWidth = 75, align = 'center',
                                        cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                first_td_rush = colDef(name = "1st TD Rush", maxWidth = 75, align = 'center',
                                       cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                first_td_pass = colDef(name = "1st TD Pass", maxWidth = 75, align = 'center',
                                       cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                QB = colDef(name = "QB", maxWidth = 75, align = 'center',
                            cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                RB = colDef(name = "RB", maxWidth = 75, align = 'center',
                            cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                WR = colDef(name = "WR", maxWidth = 75, align = 'center',
                            cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                TE = colDef(name = "TE", maxWidth = 75, align = 'center',
                            cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                DEF = colDef(name = "DEF", maxWidth = 75, align = 'center',
                             cell = color_tiles(first_td_def_all, colors = c("#59a14f", "#8cd17d",  "white", "#ff9d9a", "#e15759"))),
                first_td_all_perc = colDef(name = "% of Games", maxWidth = 75, align = 'center', format = colFormat(percent = TRUE, digits = 1))
                
              ), fullWidth = TRUE)
    
  })
  
  output$playertbl <- renderReactable({
    
    playersjoin <- playersjoin |> 
      filter(season == input$sznchoose3, offense %in% input$teaminput) |> 
      select(-season, -offense)
    
    reactable(playersjoin,
              compact = FALSE,
              pagination = TRUE,
              columns = list(
                full_name = colDef(name = "Player",
                                           maxWidth = 150,
                                           align = "left", sticky = 'left'),
                team_logo_espn = colDef(name = "",
                                        maxWidth = 35,
                                        cell = embed_img(),
                                        align = "center", sticky = 'left'),
                first_td_times = colDef(name = "Total 1st TD", maxWidth = 100, align = 'center',
                                        cell = color_tiles(playersjoin, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                first_td_rush = colDef(name = "1st TD Rush", maxWidth = 100, align = 'center',
                                       cell = color_tiles(playersjoin, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                first_td_pass = colDef(name = "1st TD Pass", maxWidth = 100, align = 'center',
                                       cell = color_tiles(playersjoin, colors = c("#e15759", "#ff9d9a", "white", "#8cd17d", "#59a14f"))),
                teamTDperc = colDef(name = "% of Team's First TDs", maxWidth = 100, align = 'center', format = colFormat(percent = TRUE, digits = 1))
                
              ), fullWidth = TRUE, defaultPageSize = 25)
    
    
  })
      
    
}

# Run the application 
shinyApp(ui = ui, server = server)
