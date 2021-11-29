#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the front end UI to explore nfl game data and predict a winner 
#

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(plotly)

# link to the dataset
originalDataLink <- "https://www4.stat.ncsu.edu/~online/datasets/"

# team data, shows offensive and defensive data throughout a season
teamData <- read_csv("data/teamData.csv")

# game data for the visuals
gameVisualData <- read_csv("data/visualGameData.csv")

# data set of the schedule
schedule <- read_csv("data/schedule.csv")

# week vector. Using weeks from the dataframe gets thrown off by the bye weeks
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)

# visual game data
#visualData <- read_csv("data/visualGameData.csv")

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "NFL",
  
  # create tabs for the sections
  tabsetPanel(
    #############
    # About tab #
    #############
    
      tabPanel(
        title = "About",
        
        mainPanel(
          img(
            src = "nfl-logo.png",
            height = "203px",
            width = "320px"
          ),
          
          h3("This App's Purpose"),
          "The purpose of this app is to examine NFL game data and explore different 
          relationships within the data and the outcome of a game.",
          
          h3("Data"),
          # original data set information
          "The data comes from the 2002-2014 seasons and has both regular and postseason games in each season. 
          The data was extracted from ESPN using web scraping and the file can be located",
          a(href = originalDataLink, "here"),
          " under scoresFull.csv",
          
          # create a new paragraph explaining the manipulation done to the data set
          br(),
          
          " ",
          
          h3("Tabs"),
          
          tags$ul(
            tags$li("Data: Show the raw data used throughout the app"),
            
            tags$li("Data Exploration: Allows the user to visualize and summarize the data"),
            
            tags$li("Modeling: Information about 3 different models that are used to predict a winner based on different inputs")
          )
          
        ),

      ),
      ############
      # Data tab #
      ############
      
      tabPanel(
        title = "Data",
        
        sidebarPanel(
          # possibly give the user the option to choose the original game data or the team data
          ## the data is the same but displayed in two different ways 
          
          ### team data
          pickerInput(
            inputId = "teamFilter",
            label = "Select the team(s)",
            choices = unique(teamData$team),
            selected = unique(teamData$team),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE,
                                    liveSearch = TRUE),
          ),
          
          pickerInput(
            inputId = "seasonFilter",
            label = "Select season(s)",
            choices = unique(teamData$season),
            selected = unique(teamData$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "weekFilter",
            label = "Select week(s)",
            choices = weeks,
            selected = weeks,
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "columnFilter",
            label = "Select column(s)",
            choices = colnames(teamData),
            selected = colnames(teamData),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE,
                                    liveSearch = TRUE)
          ),
          
          downloadButton("downloadData", "Download")
        ),
        
        mainPanel(
          dataTableOutput("dataTable")
        )
      ),
      ########################
      # Data Exploration tab #
      ########################
      
      tabPanel(
        title = "Data Exploration",
        
        # data exploration side bar
        sidebarPanel(
          
          radioButtons(
            inputId = "dataset",
            label = "Choose a data set",
            choiceNames = c("Team Data", "Game Data"),
            choiceValues = c("teamDat", "gameDat"),
            selected = "teamDat",
            inline = TRUE
            
          ),
          
          ### Team Data options
          conditionalPanel(
            condition = "input.dataset == 'teamDat'",
            
            ### Variables for all graphs/summaries
            h3("Universal Variables"),
            
            pickerInput(
              inputId = "seasonsFilterTeam",
              label = "Season(s)",
              choices = unique(teamData$season),
              selected = unique(teamData$season),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ),
            
            # pickerInput(
            #   inputId = "conferenecFilterTeam",
            #   label = "Conference(s)",
            #   choices = sort(unique(teamData$conference)),
            #   selected = unique(teamData$conference),
            #   multiple = TRUE
            # ),
            # 
            # pickerInput(
            #   inputId = "divisionFilterTeam",
            #   label = "Division(s)",
            #   choices = sort(unique(teamData$division)),
            #   selected = unique(teamData$division),
            #   multiple = TRUE,
            #   options = pickerOptions(actionsBox = TRUE)
            # ),
            
            pickerInput(
              inputId = "weeksFilterTeam",
              label = "Week(s)",
              choices = weeks,
              selected = weeks,
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ),
            
            pickerInput(
              inputId = "teamsFilterTeam",
              label = "Team(s)",
              choices = unique(teamData$team),
              selected = unique(teamData$team),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            ),
            
            ### graph inputs
            h3("Graph Options"),
            
            ### type of graphical summaries
            radioButtons(
              inputId = "plotTypeTeam",
              label = "Plot Type",
              # histogram and scatter plot for now. Not sure what they'll be
              choiceValues = c("lineGraphTeam", "scatterPlotTeam"),
              choiceNames = c("Line Graph", "Scatter Plot"),
              selected = "lineGraphTeam",
              inline = TRUE
            ),
            
            # histogram/first plot options
            conditionalPanel(
              # condition for the plot type
              condition = "input.plotTypeTeam == 'lineGraphTeam'",
              
              # some option for whatever this plot is going to be
              selectInput(
                inputId = "lineVarTeam",
                label = "Select the stat",
                choices = colnames(teamData)[6:46],
                selected = "offPassYds",
                multiple = FALSE
              )
            ),
            
            # scatter plot options
            conditionalPanel(
              # condition for the plot type
              condition = "input.plotTypeTeam == 'scatterPlotTeam'",
              
              # select the x variable for the scatter plot
              selectInput(
                inputId = "xVarTeam",
                label = "Select an X variable",
                choices = colnames(teamData)[6:46],
                selected = "offTotalYds"
              ),
              
              # select the y variable for the scatter plot
              selectInput(
                inputId = "yVarTeam",
                label = "Select a Y variable",
                choices = colnames(teamData)[6:46],
                selected = "defTotalYds"
              )
            ),
            
            h3("Numerical Options"),
            
            ### summary options
            radioButtons(
              inputId = "summaryTypeTeam",
              label = "Summary Type",
              # numeric and something else as the numerical summaries
              choiceValues = c("numericTeam", "totAvgTeam"),
              choiceNames = c("Numeric", "Totals and Averages"),
              selected = "numericTeam",
              inline = TRUE
            ),
            
            # numeric summary options
            conditionalPanel(
              condition = "input.summaryTypeTeam == 'numericTeam'",
              
              # select the variables for the numeric summary table
              pickerInput(
                inputId = "numVarsTeam",
                label = "Select the variable(s) to summarize",
                # excludes week, season, team, win columns
                choices = colnames(teamData)[6:46],
                selected = colnames(teamData)[6:46],
                multiple = TRUE,
                options = pickerOptions(actionsBox = TRUE,
                                        liveSearch = TRUE)
              )
            ),
          
            conditionalPanel(
              condition = "input.summaryTypeTeam == 'totAvgTeam'",
              
              numericInput(
                inputId = "avgRounding",
                label = "Number of digits to round the averages",
                value = 2,
                min = 0,
                max = 10,
                step = 1
              )
            ),
          

          ), # end of conditional panel for the team data
          
          ### game data options
          conditionalPanel(
            condition = "input.dataset == 'gameDat'",
            
            h3("Universal Variables"),
            
            pickerInput(
              inputId = "seasonFilterGame",
              label = "Season(s)",
              choices = unique(gameVisualData$season),
              selected = unique(gameVisualData$season),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ),
            
            pickerInput(
              inputId = "weekFilterGame",
              label = "Week(s)",
              choices = weeks,
              selected = weeks,
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ),
            
            pickerInput(
              inputId = "homeTeamFilterGame",
              label = "Home Team(s)",
              choices = unique(gameVisualData$homeTeam),
              selected = unique(gameVisualData$homeTeam),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            ),
            
            pickerInput(
              inputId = "awayTeamFilterGame",
              label = "Away Team(s)",
              choices = sort(unique(gameVisualData$awayTeam)),
              selected = unique(gameVisualData$awayTeam),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            ),
            
            h3("Graph Options"),
            
            radioButtons(
              inputId = "plotTypeGame",
              label = "Plot Type",
              choiceValues = c("scatterPlotGame", "otherGame"),
              choiceNames = c("Scatter Plot", "Something Else"),
              selected = "scatterPlotGame"
            )
            
          ) # end of conditional panel for game Data 

        ),
        
        # data exploration main panel
        mainPanel(
          # graphs
          h3("Visual Graphs"),
          
          conditionalPanel(
            # graphs for the team dataset
            condition = "input.dataset == 'teamDat'", 
            
            conditionalPanel(
              condition = "input.plotTypeTeam == 'scatterPlotTeam'",
              plotlyOutput("scatterPlotTeam")
            ),
            
            conditionalPanel(
              condition = "input.plotTypeTeam == 'lineGraphTeam'",
              plotlyOutput("lineGraphTeam")
            ),
          ),
          

          
          # numeric summaries
          h3("Numerical Summaries"),
          
          conditionalPanel(
            condition = "input.dataset == 'teamDat'",
            
            conditionalPanel(
              condition = "input.summaryTypeTeam == 'numericTeam'",
              dataTableOutput("numericSummaryTeam")
            )
          ),
          
          
        ) # end of main panel for data exploration tab
      ),
    
      #####  Modeling tab
      navbarMenu(
        title = "Modeling",
      
        # modeling info tab within modeling
        tabPanel(
          title = "Modeling Info",
          mainPanel(fluidPage(
            
            br(),
            
            h3("Goals of the models"),
            
            "The goal of the model section is to determine who will win an NFL game. This is done by classification to see if the home 
            team wins or loses (away team wins). There will be 3 different types of models that will 
            be used to classify a winner for a game: logistic regression, random forests, and a classification tree",
            
            br(),
            
            "These models will be predicting for a single week in an NFL season. The user will select a single week to predict the 
            winners for, along with the number of weeks that will be used to train the models for classification.",
            
            br(),
            
            h2("Initial Thoughts about the models"),
            
            "Putting my initial thoughts about what is going to happen with the modeling.
          The user is going to be predicting for a single week in a season. They'll first choose the season they want,
          then the week, and finally the number of weeks they want to use as their 'training' set 
          (basically number of weeks before that week). This will filter the data into a training and test set in the server file.
          Once that's all set, the user will choose the variables they want in each of the models to predict that week's results. 
          The values in the training set will need to be aggregated throughout the weeks",
            
            br(),
            br(),
            
            "A few other notes quick. When selecting the weeks for the training and test sets, they should have at least 1 week in
          the training. So they need to select at least week 2 for prediction and 1 week of training within that season. They can
          select 21 (i.e. the super bowl) and use the whole season, but can't use a number higher than the predicted week. So it
          will need to be a maximum of the input if possible. 
          These weeks before the prediction will need to be aggregated throughout to update how well the team is doing. 
          The first week  in the time range they select is basically the starting point of what they're interested in. 
          So we don't care about anything that happened before it. The elo rating (if they choose to use it) will be the 
          only thing that takes anything outside this window into account.",
            
            h4("Logistic Regression"),
            
            "Talk about the benefits and drawback of the approach. And some math stuff explaining it.",
            
            br(),
            
            "Equation:",
            uiOutput("logRegEq"),
            
            br(),
            br(),
            
            h4("Random Forests"),
            
            "Talk about the benefits and drawback of the approach. And some math stuff explaining it.",
            
            br(),
            
            h4("Classification Trees"),
            
            "Talk about the benefits and drawback of the approach. And some math stuff explaining it."
            
          )),
          

        ),
        
        # model fitting tab within modeling
        tabPanel(
          title = "Model Fitting",
          sidebarPanel(
            
            numericInput(
              inputId = "seed",
              label = "Select a seed for replication",
              value = 55,
              min = -1000,
              max = 1000,
              step = 1
            ),
            
            selectInput(
              inputId = "seasonModel",
              label = "Select a season",
              choices = unique(teamData$season),
              selected = "2014",
              multiple = FALSE
            ),
            
            # this is the week that'll be predicted, must be at least 2 to ensure that there is data from
            # the same season to be used as training
            numericInput(
              inputId = "weekModel",
              label = "Please select a week to predict",
              value = 8,
              min = 2,
              max = 21,
              step = 1
            ),
            
            # uses the input from weekModel to limit what the user can select
            uiOutput("trainWeeksInput"),
            
            h3("Logistic Regression"),
            
            selectInput(
              inputId = "logVars",
              label = "Select the variables to use in the logistic model",
              choices = colnames(teamData),
              selected = colnames(teamData),
              multiple = TRUE
            ),
            
            h3("Random Forest"),
            
            selectInput(
              inputId = "rfVars",
              label = "Select the variables to use in the random forest model",
              choices = colnames(teamData),
              selected = colnames(teamData),
              multiple = TRUE
            ),
            
            h3("Classification Tree"),
            selectInput(
              inputId = "treeVars",
              label = "Select the variables to use in the tree model",
              choices = colnames(teamData),
              selected = colnames(teamData),
              multiple = TRUE
            ),
            
            actionButton(
              inputId = "trainModels",
              label = "Fit Models"
            )
            
          ),
          
          mainPanel(
            "Model stuff"
          )

        ),
        
        # prediction tab within modeling
        tabPanel(
          title = "Prediction",
          
          sidebarPanel(
            radioButtons(
              inputId = "modelType",
              label = "Choose a Model to Predict",
              choiceNames = c(
                "Logistic Regression",
                "Random Forest",
                "Classification Tree"
              ),
              choiceValues = c("logReg", "randFor", "classTree"),
              selected = "logReg",
              inline = TRUE
            )
          ),
          
          mainPanel(
            "Prediction Stuff"
          )
        )
      
      )

    )
  
  )
)
