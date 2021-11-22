#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the front end UI to explore nfl game data and predict a winner 
#

library(shiny)

originalDataLink <- "https://www4.stat.ncsu.edu/~online/datasets/"

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "NFL",
  
  # create tabs for the sections
  tabsetPanel(
    
      # About tab
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
          "The data comes from the 2002-2016 seasons and has both regular and postseason games in each season. 
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
      
      # Data tab
      tabPanel(
        title = "Data"
      ),
    
      # Data Exploration tab
      tabPanel(
        title = "Data Exploration"
      ),
    
      # Modeling tab
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
