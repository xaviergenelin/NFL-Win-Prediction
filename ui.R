#
# author: Xavier Genelin
# date: 11/30/2021
# purpose: the front end UI to explore nfl game data and predict a winner 
#

library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(plotly)
library(rattle)
library(shinythemes)

# link to the dataset
originalDataLink <- "https://www4.stat.ncsu.edu/~online/datasets/"

# team data, shows offensive and defensive data throughout a season
teamData <- read_csv("data/teamData.csv", show_col_types = FALSE)

# game data for the visuals
gameVisualData <- read_csv("data/visualGameData.csv", show_col_types = FALSE)

# data set of the schedule
schedule <- read_csv("data/schedule.csv", show_col_types = FALSE)

# week vector. Using weeks from the dataframe gets thrown off by the bye weeks
weeks <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21)

# variables that will be in the models
modelVars <- c("defRushYdsDiff", "defRushTDDiff", "defPassCompDiff", "defPassAttDiff", "defPassYdsDiff",
               "defPassTDDiff","defPassIntDiff", "defTimesSackedDiff", "defSackYdsLostDiff", "defFumLostDiff",
               "defthirdPercDiff","deffourthPercDiff", "defCompPercDiff", "defRushYPCDiff", "defFirstDownsDiff",
               "defNetPassYdsDiff","defTurnoversDiff", "offRushAttDiff", "offRushYdsDiff", "offPassCompDiff",
               "offPassAttDiff","offPassYdsDiff", "offPassTDDiff", "offPassIntDiff", "offTimesSackedDiff",
               "offSackYdsLostDiff", "offFumDiff","offFumLostDiff", "offNumPenDiff", "offPenYdsDiff",
               "topDiff", "eloDiff", "offthirdPercDiff", "offfourthPercDiff" , "offCompPercDiff", "offRushYPCDiff",
               "offFirstDownsDiff", "offNetPassYdsDiff", "offTotalYdsDiff", "offTurnoversDiff","offTotalPlaysDiff",
               "offPointsDiff", "defPointsDiff")

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "NFL",
  
  theme = shinytheme("cosmo"),
  
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
          "The dataset has statistics for both teams in each game. This data set was manipulated to get single team data. The code
          used to create this dataset and other datasets used throughout the app can be found ",
          a(href = "https://github.com/xaviergenelin/NFL-Win-Prediction", "here"),
          " under Data Prep.Rmd. There was a metric added to the dataset called ",
          a(href = "https://en.wikipedia.org/wiki/Elo_rating_system", "Elo"),
          ". This is taken from ",
          a(href = "https://fivethirtyeight.com/", "FiveThrityEight"),
          " which use Elo in their own predictions. A description of how they do their predictions can be found ",
          a(href = "https://fivethirtyeight.com/methodology/how-our-nfl-predictions-work/", "here"),
          " along with other information about the Elo value.",
          
          # create a new paragraph explaining the manipulation done to the data set
          br(),
          
          " ",
          
          h3("Tabs"),
          
          tags$ul(
            tags$li("Data: Show the raw team data used throughout the app as well as the original game data"),
            
            tags$li("Data Exploration: Allows the user to visualize and summarize the data for the individual teams and for games"),
            
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
            choices = sort(unique(teamData$season)),
            selected = unique(teamData$season),
            multiple = TRUE,
            options = pickerOptions(actionsBox = TRUE)
          ),
          
          pickerInput(
            inputId = "weekFilter",
            label = "Select week(s)",
            choices = sort(unique(teamData$week)),
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

          ### Team Data options
          sidebarPanel(
            
            ### Variables for all graphs/summaries
            h3("Universal Variables"),
            
            pickerInput(
              inputId = "seasonsFilterExp",
              label = "Season(s)",
              choices = sort(unique(teamData$season)),
              selected = sort(unique(teamData$season)),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ),
            
            pickerInput(
              inputId = "conferenceFilterExp",
              label = "Conference(s)",
              choices = sort(unique(teamData$conference)),
              selected = unique(teamData$conference),
              multiple = TRUE
            ),
            
            # division filter based on conference if selected
            uiOutput("divisionFilterInput"),
            
            pickerInput(
              inputId = "weeksFilterExp",
              label = "Week(s)",
              choices = sort(unique(teamData$week)),
              selected = unique(teamData$week),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ),
            
            # team filter based on conference and division if selected
            uiOutput("teamFilterInput"),

            ### graph inputs
            h3("Graph Options"),
            
            ### type of graphical summaries
            radioButtons(
              inputId = "plotType",
              label = "Plot Type",
              choiceValues = c("lineGraph", "scatterPlot"),
              choiceNames = c("Line Graph", "Scatter Plot"),
              selected = "lineGraph",
              inline = TRUE
            ),
            
            # histogram/first plot options
            conditionalPanel(
              # condition for the plot type
              condition = "input.plotType == 'lineGraph'",
              
              # some option for whatever this plot is going to be
              selectInput(
                inputId = "lineVar",
                label = "Select the stat",
                choices = colnames(teamData)[5:52],
                selected = "offPassYds",
                multiple = FALSE
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
                choices = colnames(teamData)[5:52],
                selected = "offTotalYds"
              ),
              
              # select the y variable for the scatter plot
              selectInput(
                inputId = "yVar",
                label = "Select a Y variable",
                choices = colnames(teamData)[5:52],
                selected = "defTotalYds"
              )
            ),
            
            h3("Numerical Options"),
            
            ### summary options
            radioButtons(
              inputId = "summaryType",
              label = "Summary Type",
              # numeric and something else as the numerical summaries
              choiceValues = c("numeric", "teamSum"),
              choiceNames = c("Numeric", "Team"),
              selected = "numeric",
              inline = TRUE
            ),
            
            # numeric summary options
            conditionalPanel(
              condition = "input.summaryType == 'numeric'",
              
              # select the variables for the numeric summary table
              pickerInput(
                inputId = "numVars",
                label = "Select the variable(s) to summarize",
                # excludes week, season, team, win columns
                choices = colnames(teamData)[5:51],
                selected = colnames(teamData)[5:51],
                multiple = TRUE,
                options = pickerOptions(actionsBox = TRUE,
                                        liveSearch = TRUE)
              )
            ),
          
            conditionalPanel(
              condition = "input.summaryType == 'teamSum'",
              
              numericInput(
                inputId = "avgRounding",
                label = "Number of digits to round the averages",
                value = 2,
                min = 0,
                max = 10,
                step = 1
              )
            )
          
        ),
        
        # data exploration main panel
        mainPanel(
          # graphs
          h3("Visual Graphs"),
            
          conditionalPanel(
            condition = "input.plotType == 'scatterPlot'",
            plotlyOutput("scatterPlot")
          ),
          
          conditionalPanel(
            condition = "input.plotType == 'lineGraph'",
            plotlyOutput("lineGraph")
          ),
          
          # numeric summaries
          h3("Numerical Summaries"),
            
          conditionalPanel(
            condition = "input.summaryType == 'numeric'",
            dataTableOutput("numericSummary")
          ),
          
          conditionalPanel(
            condition = "input.summaryType == 'teamSum'",
            DTOutput("teamSummary")
          )
          
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
            
            h2("Goals of the models"),
            
            "The goal of the model section is to determine who will win an NFL game. This is done by classification to see if the home 
            team wins or loses (away team wins). For simplicity, ties aren't considered for this data set to keep it as a binary outcome.
            There will be 3 different types of models that will 
            be used to classify a winner for a game: logistic regression, random forests, and a classification tree",
            
            br(), 
            br(),
            
            "These models will be predicting for a single week in an NFL season. The user will select a single week to predict the 
            winners for, along with the number of weeks that will be used to train the models for classification. The training data
            will be aggregated throughout to get a better understanding of how a team is playing during that stretch. These aggreagations
            will be the cummulative average for both the home and away teams. The only statistic that isn't averaged is the Elo value taken
            from 538, which already takes into account how a team has been performing.",
            
            br(),
            br(),

            "For the modeling, these average stats and the Elo value will be compared between the two teams in the game. Each comparison will be a difference
            between the home team's stat and the away team's stat.",
            
            h3("Logistic Regression"),
            
            "A logistic regression is a model that is used for binary classification. A binary case is when there
            are only two different classes. The logistic model will calculate the probability, or log odds, of an 
            event occuring or not. If this probability of the event is more likely to occur, probability of 0.5 or more,
            then we classify this as the event occuring. If it's below 0.5, then we classify it as the event not occuring.
            The logistic model will be modeled with the following formula:",
            
            br(),
            uiOutput("logRegEq"),
            
            br(),
            
            "One of the advantages of this approach is the simplicity and interpretability of it. Because this is one of the more
            simple models, it is easy to implement it for classification. One of the drawbacks to this is that if you have more
            features, or variables, than you do observations in the dataset, your model will overfit the data.",
            
            br(),
            
            h3("Classification Trees"),
            
            "A tree can be used in both classification and regression. For both instances, the algorithm recursively splits the variables
            into different regions that best describe the data. The data continuely gets split into groups until we reach a point where we 
            can't break this down any further, or when breaking this down wouldn't lead to any additional information gained. The splits 
            are made to reduce the training error as much as possible at that split. We then get 
            an output at the end of all the branches, which are called terminal nodes. Because we're interested in binary classification, 
            the output will be one of two classes. The different groups are the branches of the tree and if you draw this out from the 
            initial data set, you get an image that looks like a tree. If the groups cannot be broken out distinctly,
            then we can take the majority of observations that are in that terminal node. ",
            
            br(),
            
            "One of the advantages to this method is that the data doesn't have to be normalzied or scaled like with other methods. Some other
            methods can be thrown off when variables are on different scales than each other and their results can be misleading. A decision tree
            is also another intuitive method and is easy for someone to be able to comprehend how the results are obtained. One of the big disadvantages
            to this method is that any change in the data can vastly change the structure of the model, regardless of how large or small that change is.
            Depending on the amount of variables, this can also take longer to calculate since it needs to go through all of the variables to determine the 
            best splits for each branch.",
            
            h3("Random Forests"),
            
            "Random forests expand on the idea of decision trees. It consists of a large number of individual decision trees that operate like an ensemble,
            or a forest. Each of the individual trees split out as described earlier and the most popular vote within that individual tree is that model's 
            output/prediction. This plays off of the idea of \"power in numbers\". These individual trees create a sample of our training data and each are 
            restricted to a subset of our variables. Otherwise, they would all get come to the same conclusion. For classification, this majority vote is use
            as the predicted class and for a regression, the predictions of all the trees get averaged",
            
            br(),
            br(),
            
            "One of the drawbacks to this is that it is not as interpretable as the other two methods. A single tree is easily understandable, and even a small 
            forest of only 5 or so trees could be interpreted, but the number of trees is typically much larger. Due to the large amount of trees being evaluated 
            this also increases the amount of time it takes to train a model. One of the big positives is that because they use subsets of the data, this reduces 
            the overall error and improves the accuracy of our prediction. This also automatically takes care of missing values and outliers for us. By using many 
            individual trees, this is not as sensitive to new data like individual trees are and collectively the trees are less impaced by noise.",
            
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
              choices = sort(unique(teamData$season)),
              selected = "2014",
              multiple = FALSE
            ),
            
            # this is the week that'll be predicted, must be at least 2 to ensure that there is data from
            # the same season to be used as training
            numericInput(
              inputId = "predWeek",
              label = "Select a week to predict",
              value = 8,
              min = 2,
              max = 21,
              step = 1
            ),
            
            # uses the input from weekModel to limit what the user can select
            uiOutput("trainWeeksInput"),
            
            h3("Logistic Regression"),
                      
            # logistic regression variables
            pickerInput(
              inputId = "logVars",
              label = "Logistic model variables",
              choices = sort(modelVars),
              selected = c("defRushYdsDiff", "defPassYdsDiff", "eloDiff", 
                           "defTurnoversDiff", "offPointsDiff", "defPointsDiff", 
                           "offTurnoversDiff", "defTimesSackedDiff", "offPenYdsDiff", 
                           "offRushYPCDiff", "defRushYPCDiff", "topDiff",
                           "offRushYdsDiff", "offPassYdsDiff"),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            ),
            
            h3("Classification Tree"),
            
            # classifcation tree variables
            pickerInput(
              inputId = "treeVars",
              label = "Tree model variables",
              choices = sort(modelVars),
              selected = c("defRushYdsDiff", "defPassYdsDiff", "eloDiff", 
                           "defTurnoversDiff", "offPointsDiff", "defPointsDiff", 
                           "offTurnoversDiff", "defTimesSackedDiff", "offPenYdsDiff", 
                           "offRushYPCDiff", "defRushYPCDiff", "topDiff",
                           "offRushYdsDiff", "offPassYdsDiff"),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            ),
            
            h4(tags$b("Complexity Parameter:")),
            div(
              numericInput(
                inputId = "numCp",
                label = "Number of Values",
                min = 1, 
                max = 5,
                value = 3,
                step = 1
              ),
              # puts the inputs side by side to save space
              style = "display:inline-block"
            ),
            div(
              numericInput(
                inputId = "minCp",
                label = "Min Cp",
                min = 0, 
                max = 100,
                value = 0.01
              ),
              
              # puts the inputs side by side to save space
              style = "display:inline-block"
            ),
            div(
              uiOutput("maxCpInput"),
              
              # puts the inputs side by side to save space
              style = "display:inline-block"
            ),
            
            
            h3("Random Forest"),
            
            # random forest variables
            pickerInput(
              inputId = "rfVars",
              label = "Random Forest model variables",
              choices = sort(modelVars),
              selected = c("defRushYdsDiff", "defPassYdsDiff", "eloDiff", 
                           "defTurnoversDiff", "offPointsDiff", "defPointsDiff", 
                           "offTurnoversDiff", "defTimesSackedDiff", "offPenYdsDiff", 
                           "offRushYPCDiff", "defRushYPCDiff", "topDiff",
                           "offRushYdsDiff", "offPassYdsDiff"),
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE,
                                      liveSearch = TRUE)
            ),
            
            # allows for multiple values to be selected for mtry value
            selectizeInput(
              inputId = "mtryValues",
              label = "Select up to 5 mtry values",
              choices = 1:length(modelVars),
              multiple = TRUE,
              selected = c(3, 5, 10),
              options = list(maxItems = 5)
            ),
            
            br(),
            
            actionButton(
              inputId = "trainModels",
              label = "Fit Models"
            )
            
          ),
          
          mainPanel(
            # Overall Accuracies for the models
            h3("Test Accuracies"),
            dataTableOutput("accuracyResults"),
            
            # Logistic Regression Results
            h3("Logistic Summary"),
            dataTableOutput("logSummary"),
            
            # Diagram of the best tree model
            h3("Tree Summary"),
            plotOutput("treeSummary"),
            
            # Most important variables for the random forest model
            h3("Random Forest Summary"),
            plotOutput("rfSummary")
          )

        ),
        
        # prediction tab within modeling
        tabPanel(
          title = "Prediction",
          
          sidebarPanel(
            # select the type of model they want to predict for 
            radioButtons(
              inputId = "modelType",
              label = "Choose a Model to Predict",
              choiceNames = c(
                "Logistic Regression",
                "Classification Tree",
                "Random Forest"
              ),
              choiceValues = c("logReg", "classTree", "randFor"),
              selected = "logReg",
              inline = TRUE
            ),
            
            # start the prediction based on the inputs below
            actionButton(
              inputId = "startPrediction",
              label = "Make Prediction"
            ),
            
            # create a little space between the prediciton button and the variables
            br(),
            br(),
            
            conditionalPanel(
              condition = "input.modelType == 'logReg'",
              uiOutput("logPredVariables")
            ),
            
            conditionalPanel(
              condition = "input.modelType == 'classTree'",
              uiOutput("treePredVariables")
            ),
            
            conditionalPanel(
              condition = "input.modelType == 'randFor'",
              uiOutput("rfPredVariables")
            )

          ),
          
          mainPanel(
            h3("Prediction Resutls"),
            dataTableOutput("userPred")
            
          )
        )
      
      )

    )
  
  )
)
