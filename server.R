#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the back end code to explore nfl game data and predict a winner 
#

library(shiny)
library(DT)

nflData <- read_csv("data/nflData.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  ############
  # Data tab #
  ############
  
  # subset the data on the data tab if the user wants any specific filters
  newData <- reactive({
    newData <- nflData %>% filter(season %in% input$seasonFilter,
                                  Team %in% input$teamFilter,
                                  week %in% input$weekFilter)
  })
  
  # data table in the data tab
  output$dataTable <- renderDataTable({
    
    newData()
    
  })
  
  # download data from download button
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("data.csv")
    },
    content = function(file){
      write.csv(nflData %>% filter(season %in% input$seasonFilter,
                                   Team %in% input$teamFilter,
                                   week %in% input$weekFilter),
                file,
                row.names = FALSE)
    }
  )
  ########################
  # Data Exploration Tab #
  ########################

})

