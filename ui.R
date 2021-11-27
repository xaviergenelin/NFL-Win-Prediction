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

# the original nfl data
#gameData <- read_csv("")

# the manipulated nfl data
teamData <- read_csv("data/teamData.csv")

weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "NFL",
  
  # create tabs for the sections
  tabsetPanel(
    
    ##### About tab
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
      
      ##### Data tab
      tabPanel(
        title = "Data",
        
        sidebarPanel(
          # possibly give the user the option to choose the original game data or the team data
          ## the data is the same but displayed in two different ways 
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
    
      ##### Data Exploration tab
      tabPanel(
        title = "Data Exploration",
        
        # data exploration side bar
        sidebarPanel(
          
          ### Variables for all graphs/summaries
          h3("Universal Variables"),
          
          pickerInput(
            inputId = "teamsFilter",
            label = "Team(s)",
            choices = unique(teamData$team),
            selected = unique(teamData$team),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE,
                                    liveSearch = TRUE)
          ),
          
          pickerInput(
            inputId = "seasonsFilter",
            label = "Season(s)",
            choices = unique(teamData$season),
            selected = unique(teamData$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "weeksFilter",
            label = "Week(s)",
            choices = weeks,
            selected = weeks,
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          ### graph inputs
          h3("Graph Options"),
          
          ### type of graphical summaries
          radioButtons(
            inputId = "plotType",
            label = "Plot Type",
            # histogram and scatter plot for now. Not sure what they'll be
            choiceValues = c("histogram", "scatterPlot"),
            choiceNames = c("Histogram", "Scatter Plot"),
            selected = "histogram"
          ),
          
          # histogram/first plot options
          conditionalPanel(
            # condition for the plot type
            condition = "input.plotType == 'histogram'",
            
            # some option for whatever this plot is going to be
            selectInput(
              inputId = "histVars",
              label = "Are you sure about that?",
              choices = c("Yes", "No")
            )
          ),
          
          # scatter plot options
          conditionalPanel(
            # condition for the plot type
            condition = "input.plotType == 'scatterPlot'",
            
            # select the x variable for the scatter plot
            selectInput(
              inputId = "xVar",
              label = "Select an X variable",
              choices = colnames(teamData)[4:50],
              selected = "offTotalYds"
            ),
            
            # select the y variable for the scatter plot
            selectInput(
              inputId = "yVar",
              label = "Select a Y variable",
              choices = colnames(teamData)[4:50],
              selected = "defTotalYds"
            )
          ),
          
          h3("Numerical Options"),
          
          ### summary options
          radioButtons(
            inputId = "summaryType",
            label = "Summary Type",
            # numeric and something else as the numerical summaries
            choiceValues = c("numeric", "other"),
            choiceNames = c("Numeric", "Second Summary"),
            selected = "numeric"
          ),
          
          # numeric summary options
          conditionalPanel(
            condition = "input.summaryType == 'numeric'",
            
            # select the variables for the numeric summary table
            pickerInput(
              inputId = "numVars",
              label = "Select the variable(s) to summarize",
              # excludes week, season, team, win columns
              choices = colnames(teamData)[4:50],
              selected = colnames(teamData)[4:50],
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            )
          ),
          
          conditionalPanel(
            condition = "input.summaryType == 'other'",
            
            selectInput(
              inputId = "otherVars",
              label = "What summary is this going to be?",
              choices = c("No idea", "Kind of have an idea", "Lol I have no clue")
            )
          )
          
        ),
        
        # data exploration main panel
        mainPanel(
          # graphs
          conditionalPanel(
            condition = "input.plotType == 'scatterPlot'",
            plotlyOutput("scatterPlot")
          ),
          
          # numeric summaries
          conditionalPanel(
            condition = "input.summaryType == 'numeric'",
            dataTableOutput("numericSummary")
          )
        )
      ),
    
      #####  Modeling tab
      navbarMenu(
        title = "Modeling",
      
        # modeling info tab within modeling
        tabPanel(
          title = "Modeling Info",
          mainPanel(fluidPage(
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
            
          )),
          

        ),
        
        # model fitting tab within modeling
        tabPanel(
          title = "Model Fitting",
          
          selectInput(
            inputId = "seasonModel",
            label = "Select a season",
            choices = unique(teamData$season),
            selected = "2014",
            multiple = FALSE
          ),
          
          numericInput(
            inputId = "weekModel",
            label = "Please select a week to predict",
            value = 8,
            min = 3,
            max = 21,
            step = 1
          ),
          
          numericInput(
            inputId = "trainWeeks",
            label = "Select the number of weeks to use as training",
            value = 4,
            min = 2, 
            max = 8,
            step = 1
          ),
          
          selectInput(
            inputId = "logVars",
            label = "Select the variables to use in the logistic model",
            choices = colnames(teamData),
            selected = colnames(teamData),
            multiple = TRUE
          ),
          
          selectInput(
            inputId = "rfVars",
            label = "Select the variables to use in the random forest model",
            choices = colnames(teamData),
            selected = colnames(teamData),
            multiple = TRUE
          ),
          
          selectInput(
            inputId = "treeVars",
            label = "Select the variables to use in the tree model",
            choices = colnames(teamData),
            selected = colnames(teamData),
            multiple = TRUE
          )
        ),
        
        # prediction tab within modeling
        tabPanel(
          title = "Prediction"
        )
      
      )

    )
  
  )
)
