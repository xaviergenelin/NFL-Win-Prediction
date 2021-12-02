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
  
  confData <- reactive({
    teamData %>%
      filter(season %in% input$seasonsFilterTeam,
             conference %in% input$conferenecFilterTeam)
  })
  
  output$divisionFilterInput <- renderUI({
    
    pickerInput(
        inputId = "divisionFilterTeam",
        label = "Division(s)",
        choices = sort(unique(confData()$division)),
        selected = unique(confData()$division),
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE)
    )
  })
  
  divData <- reactive({
    confData() %>% filter(division %in% input$divisionFilterTeam)
  })
  
  output$teamFilterInput <- renderUI({
    pickerInput(
      inputId = "teamsFilterTeam",
      label = "Team(s)",
      choices = unique(divData()$team),
      selected = unique(divData()$team),
      multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE,
                              liveSearch = TRUE)
    )
  })
  
  # data based on user inputs
  graphDataTeam <- reactive({
    teamData %>%
      filter(season %in% input$seasonsFilterTeam,
             conference %in% input$conferenecFilterTeam,
             team %in% input$teamsFilterTeam,
             division %in% input$divisionFilterTeam,
             week %in% input$weeksFilterTeam)
  })
  
  
  # line graph
  output$lineGraphTeam <- renderPlotly({
    
    linePlot <- ggplot(data = graphDataTeam(), aes(x = date, group = team, color = team)) +
      geom_line(aes_string(y = input$lineVarTeam)) +
      scale_color_manual(values = nflColors, aesthetics = c("color")) +
      labs(title = paste0("Plot of ", input$lineVarTeam))
    
    ggplotly(linePlot, tooltip = c("x", "y", "group"))
  })

  
  # scatter plot of team data
  output$scatterPlotTeam <- renderPlotly({

    plot <- ggplot(data = graphDataTeam(), aes_string(x = input$xVarTeam, y = input$yVarTeam)) +
      geom_jitter(aes(color = win)) +
      scale_color_manual(values = c("#d7181c", "#2c7bb6")) + 
      labs(title = paste0("Plot of ", input$xVarTeam, " vs. ", input$yVarTeam))
    
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
    
    newData <- read_csv("data/teamData.csv", show_col_types = FALSE)
    
    ############!!!!!!!!!!!!!!!!!!!!!!!!!!!#################### 
    # something gets messed up here if I use teamData instead of newData
    teamSumData <- newData %>%
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
      label = "Select the number of weeks to use as training",
      value = input$predWeek - 1,
      # use 2 weeks of training to ensure that a team will have data if they select a time during a team's bye week
      # if they want to predict for week 2 then they can only use week 1 data for training
      min = ifelse(input$predWeek == 2, 1, 2), 
      max = input$predWeek - 1,
      step = 1
    )
  })
  
  output$maxCpInput <- renderUI({
    
    # default value at 10
    value <- 10
    
    # check if the minimum cp input is greater than the default
    # if it is, make the default max value the nearest integer of the minimum value + 1
    if(input$minCp > value){
      value <- round(input$minCp + 1, digits = 0)
    }
    
    numericInput(
      inputId = "maxCp",
      label = "Max Cp",
      min = input$minCp,
      max = 1000,
      value = value
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
    
    nflData <- teamData %>% 
      filter(season == input$seasonModel, 
             between(week, input$predWeek - input$trainWeeks, input$predWeek))
    
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
      arrange(season, team, week) %>%
      select(-c(win, division, conference))
    
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
    progress$inc(0.3, detail = "Fitting Logistic Regression")

    logModel <- train(homeWin ~ .,
                      data = trainData[, c(c("homeWin"), input$logVars)],
                      method = "glm",
                      family = "binomial",
                      metric = "Accuracy"
                       )

    # train the rf model
    progress$inc(0.5, detail = "Fitting Random Forest")

    mtryVals <- as.numeric(input$mtryValues)

    rfModel <- train(homeWin ~ .,
                     data = trainData[, c(c("homeWin"), input$rfVars)],
                     method = "rf",
                     metric = "Accuracy",
                     tuneGrid = expand.grid(mtry = mtryVals))


    # train the tree model
    progress$inc(0.7, detail = "Fitting Classification Tree")

    cpVals <- seq(input$minCp, input$maxCp, length.out = input$numCp)

    treeModel <- train(homeWin ~ .,
                       data = trainData[, c(c("homeWin"), input$treeVars)],
                       method = "rpart",
                       metric = "Accuracy",
                       tuneGrid = expand.grid(cp = cpVals))

    # test the models on the test set
    progress$inc(0.85, detail = "Evaluating the test set performacne")

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

