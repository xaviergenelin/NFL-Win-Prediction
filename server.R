#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the back end code to explore nfl game data and predict a winner 
#

library(shiny)
library(DT)
library(plotly)

# load in the team data
teamData <- read_csv("data/teamData.csv")

teamData$win <- as.factor(teamData$win)
teamData$week <- as.numeric(teamData$week)
teamData$date <- as.Date(teamData$date)

# game data for the visuals
gameVisualData <- read_csv("data/visualGameData.csv")

# dataset of the schedule
schedule <- read_csv("data/schedule.csv")

nflColors <- c("Arizona Cardinals" = "#97233f", "Atlanta Falcons" = "#a71930", 
               "Baltimore Ravens" = "#241773", "Buffalo Bills" = "#00338d", 
               "Carolina Panthers" = "#0085ca", "Chicago Bears" = "#0b162a", 
               "Cincinnati Bengals" = "#000000", "Cleveland Browns" = "#fb4f14", 
               "Dallas Cowboys" = "#002244", "Denver Broncos" = "#002244", 
               "Detroit Lions" = "#005a8b", "Green Bay Packers" = "#203731", 
               "Houston Texans" = "#03202f", "Indianapolis Colts" = "#002c5f", 
               "Jacksonville Jaguars" = "#000000", "Kansas City Chiefs" = "#e31837", 
               "Los Angeles Chargers" = "#002244", "Los Angeles Rams" = "#002244", 
               "Miami Dolphins" = "#008e97", "Minnesota Vikings" = "#4f2683", 
               "New England Patriots" = "#002244", "New Orleans Saints" = "#9f8958", 
               "New York Giants" = "#0b2265", "New York Jets" = "#125740", 
               "Oakland Raiders" = "#a5acaf", "Philadelphia Eagles" = "#004953", 
               "Pittsburgh Steelers" = "#000000", "San Francisco 49ers" = "#aa0000", 
               "Seattle Seahawks" = "#002244", "Tampa Bay Buccaneers" = "#d50a0a", 
               "Tennessee Titans" = "#002244", "Washington Redskins" = "#773141")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ############
  # Data tab #
  ############
  
  # data table in the data tab
  output$dataTable <- renderDataTable({
    
    newData <- teamData %>% 
      filter(season %in% input$seasonFilter,
             team %in% input$teamFilter,
             week %in% input$weekFilter) %>%
      arrange(season, team, week) %>%
      select(input$columnFilter)
    
  })
  
  # download data from download button
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data.csv")
    },
    content = function(file){
      write.csv(teamData %>% filter(season %in% input$seasonFilter,
                                   team %in% input$teamFilter,
                                   week %in% input$weekFilter) %>%
                  arrange(season, team, week) %>%
                  select(input$columnFilter),
                file,
                row.names = FALSE)
    }
  )
  ########################
  # Data Exploration Tab #
  ########################
  
  ### Team
  
  # data based on user inputs
  graphDataTeam <- reactive({
    teamData %>%
      filter(team %in% input$teamsFilterTeam,
             season %in% input$seasonsFilterTeam,
             week %in% input$weeksFilterTeam)
  })
  
  
  # line graph
  output$lineGraphTeam <- renderPlotly({
    
    linePlot <- ggplot(data = graphDataTeam(), aes(x = date, group = team, color = team)) +
      geom_line(aes_string(y = input$lineVarTeam)) +
      scale_color_manual(values = nflColors, aesthetics = c("color"))
    
    ggplotly(linePlot, tooltip = c("x", "y", "group"))
  })

  
  # scatter plot of team data
  output$scatterPlotTeam <- renderPlotly({

    plot <- ggplot(data = graphDataTeam(), aes_string(x = input$xVarTeam, y = input$yVarTeam)) +
      geom_jitter(aes(color = win)) +
      scale_color_manual(values = c("#d7181c", "#2c7bb6"))
    
    ggplotly(plot)
    
  })
  
  
  # the numerical summary for different variables
  output$numericSummaryTeam <- renderDT({
    
    # filter the data based on the universal filters and remove the week, season, and team columns
    filteredData <- teamData %>%
      filter(team %in% input$teamsFilterTeam,
             season %in% input$seasonsFilterTeam,
             week %in% input$weeksFilterTeam) %>%
      select(-c(week, season, team)) %>%
      select(input$numVarsTeam)
    
    numericSum <- do.call(cbind, lapply(filteredData, summary))
    
    as.data.frame(t(numericSum))
  })
  
  # the team summary table
  output$teamSummary <- renderDT({
    
    # filter the 
    teamSumData <- teamData %>%
      filter(team %in% input$teamsFilterTeam,
             season %in% input$seasonsFilterTeam,
             week %in% input$weeksFilterTeam) %>%
      select(-c(date,  division, conference, defRushAtt, defPassComp, defPassAtt, defSackYdsLost, 
                defNetPassYds, offRushAtt, offPassAtt, offSackYdsLost, offNetPassYds, elo, top, 
                def3rdPerc, def4thPerc, off3rdPerc, off4thPerc, offPenYds))
    
    teamSumData$win <- as.numeric(teamSumData$win)
    
    results <- teamSumData %>%
      group_by(team, season) %>%
      summarize(
        "Wins" = sum(win),
        "Games Played" = length(win),
        # offensive stats
        "Total Points" = sum(offPoints),
        "Off PPG" = round(mean(offPoints), input$avgRounding),
        # defensive stats
        "Total Points Against" = sum(defPoints),
        "Def PPG" = round(mean(defPoints), input$avgRounding)
      ) %>%
      arrange(season, team)
    
    results <- results %>%
      group_by(season) %>%
      mutate("Off PPG Rank" = order(order(`Off PPG`, decreasing = TRUE)),
             "Def PPG Rank" = order(order(`Def PPG`, decreasing = FALSE))
      ) 
    
    results <- results[, c("season", "team", "Wins", "Games Played", "Total Points", "Off PPG", 
                           "Off PPG Rank", "Total Points Against", "Def PPG", "Def PPG Rank")]

  })
  
  #####################
  # Data Modeling Tab #
  #####################
  
  ##############
  # Model Info #
  ##############
  
  output$logRegEq <- renderUI({
    
    withMathJax(
      helpText(
        "$$\\ln(\\frac{p_i}{1-p_i}) = \\beta_0 + \\Sigma^k_{j=1}\\beta_jx_{ij}$$"
      )
    )
  })
  
  #################
  # Model Fitting #
  #################
  
  output$trainWeeksInput <- renderUI({
    
    numericInput(
      inputId = "trainWeeks",
      label = "Select the number of weeks to use as training",
      value = input$weekModel - 1,
      # use 2 weeks of training to ensure that a team will have data if they select a time during a team's bye week
      # if they want to predict for week 2 then they can only use week 1 data for training
      min = ifelse(input$weekModel == 2, 1, 2), 
      max = input$weekModel - 1,
      step = 1
    )
  })

})

