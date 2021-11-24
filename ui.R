#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the front end UI to explore nfl game data and predict a winner 
#

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)

# link to the dataset
originalDataLink <- "https://www4.stat.ncsu.edu/~online/datasets/"

# the original nfl data
#gameData <- read_csv("")

# the manipulated nfl data
nflData <- read_csv("data/nflData.csv")

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
            choices = unique(nflData$Team),
            selected = unique(nflData$Team),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE,
                                    liveSearch = TRUE),
          ),
          
          pickerInput(
            inputId = "seasonFilter",
            label = "Select season(s)",
            choices = unique(nflData$season),
            selected = unique(nflData$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "weekFilter",
            label = "Select week(s)",
            choices = unique(nflData$week),
            selected = unique(nflData$week),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
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
          
          # type of graphical summaries
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
              choices = c("1", "2"),
            ),
            
            # select the y variable for the scatter plot
            selectInput(
              inputId = "yVar",
              label = "Select a Y variable",
              choices = c("3", "4")
            )
          ),
          
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
            
            selectInput(
              inputId = "numVars",
              label = "Select the variable(s) to summarize",
              choices = c("Vikings", "Who Cares")
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
          
        )
      ),
    
      #####  Modeling tab
      navbarMenu(
        title = "Modeling",
      
        # modeling info tab within modeling
        tabPanel(
          title = "Modeling Info"
        ),
        
        # model fitting tab within modeling
        tabPanel(
          title = "Model Fitting"
        ),
        
        # prediction tab within modeling
        tabPanel(
          title = "Prediction"
        )
      
      )

    )
  
  )
)
