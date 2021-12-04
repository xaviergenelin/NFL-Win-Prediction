#
# author: Xavier Genelin
# date: 11/30/2021
# purpose: the back end code to explore nfl game data and predict a winner 
#

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(plotly)
library(rattle)
library(shinythemes)

# load in the team data
teamData <- read_csv("data/teamData.csv", show_col_types = FALSE)

teamData$win <- as.factor(teamData$win)
teamData$week <- as.numeric(teamData$week)
teamData$date <- as.Date(teamData$date)

# game data for the visuals
gameVisualData <- read_csv("data/visualGameData.csv", show_col_types = FALSE)

# dataset of the schedule
schedule <- read_csv("data/schedule.csv", show_col_types = FALSE)

# nfl colors to match the up with team for the line graph
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
  
  output$divisionFilterInput <- renderUI({
    
    confData <- teamData %>%
      filter(conference %in% input$conferenceFilterExp)
    
    pickerInput(
        inputId = "divisionFilterExp",
        label = "Division(s)",
        choices = sort(unique(confData$division)),
        selected = unique(confData$division),
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE)
    )
  })
  
  output$teamFilterInput <- renderUI({
    
    divData <- teamData %>%
      filter(division %in% input$divisionFilterExp)
    
    pickerInput(
      inputId = "teamsFilterExp",
      label = "Team(s)",
      choices = unique(divData$team),
      selected = unique(divData$team),
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE)
    )
  })
  
  # data based on user inputs
  newTeamData <- reactive({
    
    # the graphs show an error in the first second or two
    # this requires the filters to be available before it shows the graph
    req(input$seasonsFilterExp, input$teamsFilterExp, input$conferenceFilterExp,
        input$weeksFilterExp, input$divisionFilterExp)
    
    teamData %>%
      filter(season %in% input$seasonsFilterExp,
             conference %in% input$conferenceFilterExp,
             team %in% input$teamsFilterExp,
             division %in% input$divisionFilterExp,
             week %in% input$weeksFilterExp)
  })
  
  
  # line graph
  output$lineGraph <- renderPlotly({
    
    linePlot <- ggplot(data = newTeamData(), aes(x = date, group = team, color = team)) +
      geom_line(aes_string(y = input$lineVar)) +
      scale_color_manual(values = nflColors, aesthetics = c("color")) +
      labs(title = paste0("Plot of ", input$lineVar))
    
    ggplotly(linePlot, tooltip = c("x", "y", "group"))
  })

  
  # scatter plot of team data
  output$scatterPlot <- renderPlotly({

    plot <- ggplot(data = newTeamData(), aes_string(x = input$xVar, y = input$yVar)) +
      geom_jitter(aes(color = win)) +
      scale_color_manual(values = c("#d7181c", "#2c7bb6")) + 
      labs(title = paste0("Plot of ", input$xVar, " vs. ", input$yVar))
    
    ggplotly(plot)
    
  })
  
  
  # the numerical summary for different variables
  output$numericSummary <- renderDT({
    
    # filter the data based on the universal filters and remove the week, season, and team columns
    filteredData <- newTeamData() %>%
      select(-c(week, season, team)) %>%
      select(input$numVars)
    
    numericSum <- do.call(cbind, lapply(filteredData, summary))
    
    as.data.frame(t(numericSum))

  })
  
  # the team summary table
  output$teamSummary <- renderDT({

    teamSumData <- newTeamData() %>%
      select(-c(date,  division, conference, defRushAtt, defPassComp, defPassAtt, defSackYdsLost, 
                defNetPassYds, offRushAtt, offPassAtt, offSackYdsLost, offNetPassYds, elo, 
                def3rdPerc, def4thPerc, off3rdPerc, off4thPerc))
    
    # changes the win from factor to numeric to be able to be aggregated over the filtered period
    teamSumData$win <- as.numeric(as.character(teamSumData$win))
    
    results <- teamSumData %>%
      # first aggregate by team and season to get individual team data
      group_by(team, season) %>%
      summarize(
        "Wins" = sum(win),
        "Games" = length(win),
        # offensive points and ppg
        "Total Points" = sum(offPoints),
        "Off PPG" = round(mean(offPoints), input$avgRounding),
        # defensive points and ppg
        "Total Points Against" = sum(defPoints),
        "Def PPG" = round(mean(defPoints), input$avgRounding),
        # offensive rush yds per game
        "Off Rush Yds/G" = round(mean(offRushYds), input$avgRounding),
        # defensive rush yds per game
        "Def Rush Yds/G" = round(mean(defRushYds), input$avgRounding),
        # offensive pass yds per game
        "Off Pass Yds/G" = round(mean(offPassYds), input$avgRounding),
        # defensive pass yds per game
        "Def Pass Yds/G" = round(mean(defPassYds), input$avgRounding),
        # defensive sacks total and per game
        "Total Def Sacks" = sum(defTimesSacked),
        "Def Sacks/G" = round(mean(defTimesSacked), input$avgRounding),
        # def ints total and per game
        "Total Takeaways" = sum(defTurnovers),
        "Takeaways/G" = round(mean(defTurnovers), input$avgRounding),
        # total and avg penalties per game
        "Total Penalties" = sum(offNumPen),
        # avg penalty yds per game
        "Penalty Yds/G" = round(mean(offPenYds), input$avgRounding),
        # total giveaways
        "Total Giveaways" = sum(offTurnovers),
        "Giveaways/G" = round(mean(offTurnovers), input$avgRounding),
        "TOP/G" = round(mean(top), input$avgRounding)
        
      ) %>%
      # remove the groups to get data by season
      ungroup() %>%
      group_by(season) %>%
      # ranks of the total/avgs computed above to compare the teams and seasons that were selected
      mutate("Off PPG Rank" = order(order(`Off PPG`, decreasing = TRUE)),
             "Def PPG Rank" = order(order(`Def PPG`, decreasing = FALSE)),
             "Off Rush Yds/G Rank" = order(order(`Off Rush Yds/G`, decreasing = TRUE)),
             "Def Rush Yds/G Rank" = order(order(`Def Rush Yds/G`, decreasing = FALSE)),
             "Off Pass Yds/G Rank" = order(order(`Off Pass Yds/G`, decreasing = TRUE)),
             "Def Pass Yds/G Rank" = order(order(`Def Pass Yds/G`, decreasing = FALSE)),   
             "Sacks/G Rank" = order(order(`Def Sacks/G`, decreasing = TRUE)),
             "Takeaways/G Rank" = order(order(`Takeaways/G`, decreasing = TRUE)),
             "Penalty Yds/G Rank" = order(order(`Penalty Yds/G`, decreasing = FALSE)),
             "Penalties/G" = round(`Total Penalties`/`Games`, input$avgRounding),
             "Penalties/G Rank" = order(order(`Penalties/G`, decreasing = FALSE)),
             "Giveaways/G Rank" = order(order(`Giveaways/G`, decreasing = FALSE)),
             "TOP/G Rank" = order(order(`TOP/G`, decreasing = TRUE))
      ) 
    
    results <- results[, c("season", "team", "Wins", "Games", 
                           # offensive stats
                           "Total Points", "Off PPG", "Off PPG Rank", 
                           "Off Rush Yds/G", "Off Rush Yds/G Rank",
                           "Off Pass Yds/G", "Off Pass Yds/G Rank",
                           "TOP/G", "TOP/G Rank",
                           "Total Penalties", "Penalties/G", "Penalties/G Rank",
                           "Penalty Yds/G", "Penalty Yds/G Rank",
                           "Total Giveaways", "Giveaways/G", "Giveaways/G Rank",
                           # defensive stats
                           "Total Points Against", "Def PPG", "Def PPG Rank",
                           "Def Rush Yds/G", "Def Rush Yds/G Rank",
                           "Def Pass Yds/G", "Def Pass Yds/G Rank",
                           "Total Def Sacks", "Def Sacks/G", "Sacks/G Rank",
                           "Total Takeaways", "Takeaways/G", "Takeaways/G Rank"
    )] %>%
      arrange(season, team) 
    
    results %>% select(input$teamCols)
    
  })
  
  ######################
  # Data Modeling Tabs #
  ######################
  
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
  
  
  # ensures that 2 weeks of data will be used for training to cover for a bye week during the period selected
  # unless the user selects to predict for week 2 then it uses week 1 (or 1 week of training)
  output$trainWeeksInput <- renderUI({
    
    numericInput(
      inputId = "trainWeeks",
      label = "Select the number of prior weeks to use as training",
      value = input$predWeek - 1,
      # use 2 weeks of training to ensure that a team will have data if they select a time during a team's bye week
      # if they want to predict for week 2 then they can only use week 1 data for training
      min = ifelse(input$predWeek == 2, 1, 2), 
      max = input$predWeek - 1,
      step = 1
    )
  })
  
  ##### Model Training #####
  
  observeEvent(input$trainModels, {
    
    # add a progress button so the user knows the models are being fit
    progress <- Progress$new()
    
    # makes sure the progress bar closes once this event is over
    on.exit(progress$close())
    
    # initial message to the user
    progress$set(message = "Fitting Models", value = 0)
    
    # use the seed selected by the user to replicate results
    set.seed(input$seed)
    
    # filter the team data set based on universal variables selected
    progress$inc(0.1, detail = "Filtering the dataset")
    
    ### get the previous week's data as the current week to get the data going into the game
    
    # season average data to add to the week 1 for the next season
    seasonAvgs <- teamData %>%
      group_by(season, team) %>%
      summarize(
        defRushAtt = mean(defRushAtt)
        ,defRushYds = mean(defRushYds)
        ,defRushTD = mean(defRushTD)
        ,defPassComp = mean(defPassComp)
        ,defPassAtt = mean(defPassAtt)
        ,defPassYds = mean(defPassYds)
        ,defPassTD = mean(defPassTD)
        ,defPassInt = mean(defPassInt)
        ,defTimesSacked = mean(defTimesSacked)
        ,defSackYdsLost = mean(defSackYdsLost)
        ,defFum	 = mean(defFum)
        ,defFumLost = mean(defFumLost)
        ,def3rdPerc = mean(def3rdPerc)
        ,def4thPerc = mean(def4thPerc)
        ,defCompPerc = mean(defCompPerc)
        ,defRushYPC = mean(defRushYPC)
        ,defFirstDowns = mean(defFirstDowns)
        ,defNetPassYds = mean(defNetPassYds)
        ,defTotalYds	 = mean(defTotalYds)
        ,defTurnovers = mean(defTurnovers)
        ,offRushAtt = mean(offRushAtt)
        ,offRushYds = mean(offRushYds)
        ,offRushTD	 = mean(offRushTD)
        ,offPassComp = mean(offPassComp)
        ,offPassAtt = mean(offPassAtt)
        ,offPassYds = mean(offPassYds)
        ,offPassTD = mean(offPassTD)
        ,offPassInt = mean(offPassInt)
        ,offTimesSacked = mean(offTimesSacked)
        ,offSackYdsLost = mean(offSackYdsLost)
        ,offFum = mean(offFum)
        ,offFumLost = mean(offFumLost)
        ,offNumPen = mean(offNumPen)
        ,offPenYds = mean(offPenYds)
        ,top = mean(top)
        ,off3rdPerc = mean(off3rdPerc)
        ,off4thPerc = mean(off4thPerc)
        ,offCompPerc = mean(offCompPerc)
        ,offRushYPC = mean(offRushYPC)
        ,offFirstDowns = mean(offFirstDowns)
        ,offNetPassYds = mean(offNetPassYds)
        ,offTotalYds = mean(offTotalYds)
        ,offTurnovers = mean(offTurnovers)
        ,offTotalPlays = mean(offTotalPlays)
        ,offPoints = mean(offPoints)
        ,defPoints = mean(defPoints)
      ) %>%
      mutate(week = 1,
             season = season + 1)
    
    # elo from the previous season to use in the lag data 
    eloData <- teamData %>%
      select(season, week, team, elo) %>%
      arrange(season, team, week) %>%
      group_by(season, team) %>%
      summarize(across(everything(), last)) %>%
      ungroup() %>% 
      mutate(week = 1,
             season = season + 1) 
    
    # game date 
    dates <- teamData %>% select(season, week, team, date)
    
    # get the average data and the elo data together
    avgData <- merge(seasonAvgs, eloData, by = c("season", "team", "week")) 
    
    # get the game date onto the dataset
    avgData <- merge(avgData, dates, by = c("season", "team", "week"))
    
    # first observation for each team
    firstObs <- teamData %>%
      group_by(team) %>%
      arrange(season, team, week) %>%
      filter(row_number(team) == 1) %>%
      select(-c(division, conference, win))
    
    # add in the first row of data for each team
    avgData <- rbind(avgData, firstObs)
    
    # get the previous week's value except for the first week
    lagData <- teamData %>%
      group_by(team) %>%
      arrange(season, team, week) %>%
      mutate(
        defRushAtt = lag(defRushAtt, 1)
        ,defRushYds = lag(defRushYds, 1)
        ,defRushTD = lag(defRushTD, 1)
        ,defPassComp = lag(defPassComp, 1)
        ,defPassAtt = lag(defPassAtt, 1)
        ,defPassYds = lag(defPassYds, 1)
        ,defPassTD = lag(defPassTD, 1)
        ,defPassInt = lag(defPassInt, 1)
        ,defTimesSacked = lag(defTimesSacked, 1)
        ,defSackYdsLost = lag(defSackYdsLost, 1)
        ,defFum	 = lag(defFum, 1)
        ,defFumLost = lag(defFumLost, 1)
        ,def3rdPerc = lag(def3rdPerc, 1)
        ,def4thPerc = lag(def4thPerc, 1)
        ,defCompPerc = lag(defCompPerc, 1)
        ,defRushYPC = lag(defRushYPC, 1)
        ,defFirstDowns = lag(defFirstDowns, 1)
        ,defNetPassYds = lag(defNetPassYds, 1)
        ,defTotalYds	 = lag(defTotalYds, 1)
        ,defTurnovers = lag(defTurnovers, 1)
        ,offRushAtt = lag(offRushAtt, 1)
        ,offRushYds = lag(offRushYds, 1)
        ,offRushTD	 = lag(offRushTD, 1)
        ,offPassComp = lag(offPassComp, 1)
        ,offPassAtt = lag(offPassAtt, 1)
        ,offPassYds = lag(offPassYds, 1)
        ,offPassTD = lag(offPassTD, 1)
        ,offPassInt = lag(offPassInt,1)
        ,offTimesSacked = lag(offTimesSacked,1)
        ,offSackYdsLost = lag(offSackYdsLost,1)
        ,offFum = lag(offFum, 1)
        ,offFumLost = lag(offFumLost, 1)
        ,offNumPen = lag(offNumPen, 1)
        ,offPenYds = lag(offPenYds, 1)
        ,top = lag(top, 1)
        ,off3rdPerc = lag(off3rdPerc, 1)
        ,off4thPerc = lag(off4thPerc, 1)
        ,offCompPerc = lag(offCompPerc, 1)
        ,offRushYPC = lag(offRushYPC, 1)
        ,offFirstDowns = lag(offFirstDowns, 1)
        ,offNetPassYds = lag(offNetPassYds, 1)
        ,offTotalYds = lag(offTotalYds, 1)
        ,offTurnovers = lag(offTurnovers, 1)
        ,offTotalPlays = lag(offTotalPlays, 1)
        ,offPoints = lag(offPoints, 1)
        ,defPoints = lag(defPoints, 1)
        ,elo = elo
      ) %>%
      filter(week != 1)
    
    # remove the rows with na values
    lagData <- lagData[complete.cases(lagData),]
    
    # combine the lagged data along with the season
    teamModelData <- rbind(lagData, avgData) %>% 
      arrange(season, team,  week) %>% 
      filter(season != 2015) %>%
      select(-c(division, conference, win))
    
    # filter the data based on the input
    nflData <- teamModelData %>% 
      filter(season == input$seasonModel, 
             between(week, input$predWeek - input$trainWeeks, input$predWeek))
    
    # cumulative averages throughout the season
    aggNfl <- nflData %>%
      group_by(team) %>%
      arrange(team, week) %>%
      mutate(
        defRushAtt = cummean(defRushAtt)
        ,defRushYds = cummean(defRushYds)
        ,defRushTD = cummean(defRushTD)
        ,defPassComp = cummean(defPassComp)
        ,defPassAtt = cummean(defPassAtt)
        ,defPassYds = cummean(defPassYds)
        ,defPassTD = cummean(defPassTD)
        ,defPassInt = cummean(defPassInt)
        ,defTimesSacked = cummean(defTimesSacked)
        ,defSackYdsLost = cummean(defSackYdsLost)
        ,defFum	 = cummean(defFum)
        ,defFumLost = cummean(defFumLost)
        ,def3rdPerc = cummean(def3rdPerc)
        ,def4thPerc = cummean(def4thPerc)
        ,defCompPerc = cummean(defCompPerc)
        ,defRushYPC = cummean(defRushYPC)
        ,defFirstDowns = cummean(defFirstDowns)
        ,defNetPassYds = cummean(defNetPassYds)
        ,defTotalYds	 = cummean(defTotalYds)
        ,defTurnovers = cummean(defTurnovers)
        ,offRushAtt = cummean(offRushAtt)
        ,offRushYds = cummean(offRushYds)
        ,offRushTD	 = cummean(offRushTD)
        ,offPassComp = cummean(offPassComp)
        ,offPassAtt = cummean(offPassAtt)
        ,offPassYds = cummean(offPassYds)
        ,offPassTD = cummean(offPassTD)
        ,offPassInt = cummean(offPassInt)
        ,offTimesSacked = cummean(offTimesSacked)
        ,offSackYdsLost = cummean(offSackYdsLost)
        ,offFum = cummean(offFum)
        ,offFumLost = cummean(offFumLost)
        ,offNumPen = cummean(offNumPen)
        ,offPenYds = cummean(offPenYds)
        ,top = cummean(top)
        ,elo = elo
        ,off3rdPerc = cummean(off3rdPerc)
        ,off4thPerc = cummean(off4thPerc)
        ,offCompPerc = cummean(offCompPerc)
        ,offRushYPC = cummean(offRushYPC)
        ,offFirstDowns = cummean(offFirstDowns)
        ,offNetPassYds = cummean(offNetPassYds)
        ,offTotalYds = cummean(offTotalYds)
        ,offTurnovers = cummean(offTurnovers)
        ,offTotalPlays = cummean(offTotalPlays)
        ,offPoints = cummean(offPoints)
        ,defPoints = cummean(defPoints)
      ) %>%
      arrange(season, team, week) 
    
    # filter the schedule data set based on the universal variables selected 
    scheduleData <- schedule %>%
      filter(season == input$seasonModel,
             between(week, input$predWeek - input$trainWeeks, input$predWeek)) %>%
      select(-c(homeAbb, awayAbb))
    
    modeling <- left_join(scheduleData, aggNfl, by = c("season" = "season", "week" = "week", "date" = "date", "awayTeam" = "team"))
    
    colnames(modeling)[7:53] <- paste0(colnames(modeling)[7:53], "Away")
    
    modeling <- left_join(modeling, aggNfl, by = c("season" = "season", "week" = "week", "date" = "date", "homeTeam" = "team"))
    
    colnames(modeling)[54:100] <- paste0(colnames(modeling)[54:100], "Home")
    
    # calculate the difference between the stats for the home and away teams in each matchup
    modelData <- modeling %>%
      mutate(
        defRushYdsDiff = defRushYdsHome - defRushYdsAway
        ,defRushTDDiff = defRushTDHome - defRushTDAway
        ,defPassCompDiff = defPassCompHome - defPassCompAway
        ,defPassAttDiff = defPassAttHome - defPassAttAway
        ,defPassYdsDiff = defPassYdsHome - defPassYdsAway
        ,defPassTDDiff = defPassTDHome - defPassTDAway
        ,defPassIntDiff = defPassIntHome - defPassIntAway
        ,defTimesSackedDiff = defTimesSackedHome - defTimesSackedAway
        ,defSackYdsLostDiff = defSackYdsLostHome - defSackYdsLostAway
        ,defFumLostDiff = defFumLostHome - defFumLostAway
        ,defthirdPercDiff = def3rdPercHome - def3rdPercAway
        ,deffourthPercDiff = def4thPercHome - def4thPercAway
        ,defCompPercDiff = defCompPercHome - defCompPercAway
        ,defRushYPCDiff = defRushYPCHome - defRushYPCAway
        ,defFirstDownsDiff = defFirstDownsHome - defFirstDownsAway
        ,defNetPassYdsDiff = defNetPassYdsHome - defNetPassYdsAway
        ,defTurnoversDiff = defTurnoversHome - defTurnoversAway
        ,offRushAttDiff = offRushAttHome - offRushAttAway
        ,offRushYdsDiff = offRushYdsHome - offRushYdsAway
        ,offPassCompDiff = offPassCompHome - offPassCompAway
        ,offPassAttDiff = offPassAttHome - offPassAttAway
        ,offPassYdsDiff = offPassYdsHome - offPassYdsAway
        ,offPassTDDiff = offPassTDHome - offPassTDAway
        ,offPassIntDiff = offPassIntHome - offPassIntAway
        ,offTimesSackedDiff = offTimesSackedHome - offTimesSackedAway
        ,offSackYdsLostDiff = offSackYdsLostHome - offSackYdsLostAway
        ,offFumDiff = offFumHome - offFumAway
        ,offFumLostDiff = offFumLostHome - offFumLostAway
        ,offNumPenDiff = offNumPenHome - offNumPenAway
        ,offPenYdsDiff = offPenYdsHome - offPenYdsAway
        ,topDiff = topHome - topAway
        ,eloDiff = eloHome - eloAway
        ,offthirdPercDiff = off3rdPercHome - off3rdPercAway
        ,offfourthPercDiff = off4thPercHome - off4thPercAway
        ,offCompPercDiff = offCompPercHome - offCompPercAway
        ,offRushYPCDiff = offRushYPCHome - offRushYPCAway
        ,offFirstDownsDiff = offFirstDownsHome - offFirstDownsAway
        ,offNetPassYdsDiff = offNetPassYdsHome - offNetPassYdsAway
        ,offTotalYdsDiff = offTotalYdsHome - offTotalYdsAway
        ,offTurnoversDiff = offTurnoversHome - offTurnoversAway
        ,offTotalPlaysDiff = offTotalPlaysHome - offTotalPlaysAway
        ,offPointsDiff = offPointsHome - offPointsAway
        ,defPointsDiff = defPointsHome - defPointsAway
        ,homeWin = as.factor(homeWin)
      ) %>%
      select(-c(ends_with("Home"), ends_with("Away")))
    
    # save values from selected range to be used in the model prediction section
    write_csv(modelData, "./Fitted Models/modelData.csv")
    
    # create the training and testing data based on the week the user wants to predict for 
    testData <- modelData %>% filter(week == input$predWeek)
    trainData <- modelData %>% filter(week != input$predWeek)
    
    ### Logistic Model ###
    
    # train the logistic model
    progress$inc(0.2, detail = "Fitting Logistic Regression")

    logModel <- train(homeWin ~ .,
                      data = trainData[, c(c("homeWin"), input$logVars)],
                      method = "glm",
                      family = "binomial",
                      metric = "Accuracy"
                       )

    # train the rf model
    progress$inc(0.4, detail = "Fitting Random Forest")

    mtryVals <- as.numeric(input$mtryValues)

    rfModel <- train(homeWin ~ .,
                     data = trainData[, c(c("homeWin"), input$rfVars)],
                     method = "rf",
                     metric = "Accuracy",
                     tuneGrid = expand.grid(mtry = mtryVals))


    # train the tree model
    progress$inc(0.6, detail = "Fitting Classification Tree")

    possibleCps <- seq(from = input$cpVals[1], to = input$cpVals[2], by = 0.001)
    
    cpVals <- sample(possibleCps, size = input$numCp)

    treeModel <- train(homeWin ~ .,
                       data = trainData[, c(c("homeWin"), input$treeVars)],
                       method = "rpart",
                       metric = "Accuracy",
                       tuneGrid = expand.grid(cp = cpVals))

    # test the models on the test set
    progress$inc(0.8, detail = "Evaluating the test set performacne")

    logPred <- predict(logModel, testData, type = "raw")
    rfPred <- predict(rfModel, testData, type = "raw")
    treePred <- predict(treeModel, testData, type = "raw")
    
    # get the accuracies for each model
    accuracies <- c(mean(logPred == testData$homeWin),
                    mean(rfPred == testData$homeWin),
                    mean(treePred == testData$homeWin))
    
    # put the accuracies into a matrix and make them into a percentage
    accPerc <- t(as.matrix(accuracies)) * 100
    
    # name the columns of the matrix to match the accuracy
    colnames(accPerc) <- c("Logistic Regression", # logistic regression
                           paste0("Random Forest with mtry = ", rfModel$bestTune$mtry),
                           paste0("Tree with Cp = ", treeModel$bestTune$cp) # tree with best tuning parameter from selected
                           )
    
    # round the percentages to make them easier to read and add a percentage sign
    results <- as.data.frame(accPerc) %>%
      mutate_all(round, digits = 2) %>%
      mutate_all(paste0, sep = "%")
    
    # accuracy results for the 3 models
    output$accuracyResults <- renderDataTable({
      datatable(results)
    })
    
    # Logistic Regression Summary of coefficients
    output$logSummary <- renderDataTable({
      # round the results to 3 decimal places to make it easier to read
      round(as.data.frame(summary(logModel)$coef), 3)
    })
    
    # Important variables for the random forest model
    output$rfSummary <- renderPlot({
      # create a plot of the most important variables
      ## idea: have a button that lets the user to select their favorite team and use that color?
      ggplot(varImp(rfModel, type = 2)) +
        geom_col(fill = "#4f2683") + 
        labs(title = "Most Important Variables for the Random Forest Model")
    })
    
    # A diagram of the tree
    output$treeSummary <- renderPlot({
      fancyRpartPlot(treeModel$finalModel)
    })
    
    # save the results of the model fitting to the fitted models folder to be used in the predictions
    saveRDS(logModel, "./Fitted Models/logModel.rds")
    saveRDS(rfModel, "./Fitted Models/rfModel.rds")
    saveRDS(treeModel, "./Fitted Models/treeModel.rds")

  }) # end of the model fitting event
  
  ####################
  # Model Prediction #
  ####################
  
  # use the variables from the fitted models for prediction
  
  # logistic regression variables
  output$logPredVariables <- renderUI({
  
    # get the data used in the model fitting to get average values of the variables selected
    modelData <- read_csv("./Fitted Models/modelData.csv", show_col_types = FALSE)
    
    tags$ul(tagList(
      lapply(input$logVars, function(var){
        numericInput(
          inputId = paste0(var, "Value"),
          label = paste0("Select ", var, " Value"),
          value = round(median(pull(modelData[, var]), na.rm=TRUE), 2),
          step = 0.1
        )
      })
    ))
  }) 
  
  # random forest model variables
  output$rfPredVariables <- renderUI({
    
    # get the data used in the model fitting to get average values of the variables selected
    modelData <- read_csv("./Fitted Models/modelData.csv", show_col_types = FALSE)
    
    tags$ul(tagList(
      lapply(input$rfVars, function(var){
        numericInput(
          inputId = paste0(var, "Value"),
          label = paste0("Select ", var, " Value"),
          value = round(median(pull(modelData[, var]), na.rm=TRUE), 2),
          step = 0.1
        )
      })
    ))
  })
  
  # classification tree variables
  output$treePredVariables <- renderUI({
    
    # get the data used in the model fitting to get average values of the variables selected
    modelData <- read_csv("./Fitted Models/modelData.csv", show_col_types = FALSE)
    
    tags$ul(tagList(
      lapply(input$treeVars, function(var){
        numericInput(
          inputId = paste0(var, "Value"),
          label = paste0("Select ", var, " Value"),
          value = round(median(pull(modelData[, var]), na.rm=TRUE), 2),
          step = 0.1
        )
      })
    ))
  })
  
  # make predictions after the user clicks the button based on their inputs
  observeEvent(input$startPrediction, {
    
    # get the model based on their input
    # load in their selected model
    if (input$modelType == "logReg") {
      
      # get the variables they selected for the logistic model
      modelVars <- lapply(input$logVars, paste0, sep = "Value")
      # get the logistic model that was fit
      model <- readRDS("./Fitted Models/logModel.rds")
      
    } else if(input$modelType == "randFor"){
      
      # get the variables they selected for the random forest
      modelVars <- lapply(input$rfVars, paste0, sep = "Value")
      # get the logistic model that was fit
      model <- readRDS("./Fitted Models/rfModel.rds")
      
    } else {
      
      # get the variables they selected for the tree model
      modelVars <- lapply(input$treeVars, paste0, sep = "Value")
      # get the logistic model that was fit
      model <- readRDS("./Fitted Models/treeModel.rds")
      
    }
    
    # get a vector of their variables
    inputVars <- c()
    for(var in modelVars){
      inputVars <- c(inputVars, input[[var]])
    }
    
    inputVars <- t(matrix(inputVars))
    
    colnames(inputVars) <- str_remove_all(modelVars, pattern = "Value")
    
    userInputs <- as.data.frame(inputVars)
    
    
    # get the probability of the home team winning or losing based on the inputted values
    probPreds <- predict(model, userInputs, type = "prob")
    
    # get the class of the home team winning (1 or 0) based on the inputted values
    classPred <- predict(model, userInputs, type = "raw")
    
    # change the class to be home or away instead of 0 and 1 
    classPred <- ifelse(classPred == 1, "Home", "Away")
    
    # combine the class and probabilities into one
    prediction <- cbind(classPred, round(probPreds, 3))
    
    # make the column names more clear for the user
    colnames(prediction) <- c("Winning Team", 
                              "Predicted Prob. of Away Team Winning", 
                              "Predicted Prob. of Home Team Winning"
                              )
    
    prediction <- as.data.frame(prediction)

    # create the table that will show once the prediciton is made
    output$userPred <- renderDataTable({
      prediction
    })
    
  }) # end of the prediction event

})

