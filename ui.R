#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = "NFL",
  
  # create tabs for the sections
  tabsetPanel(
    
      # About tab
      tabPanel(
        title = "About"
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
