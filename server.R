#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the back end code to explore nfl game data and predict a winner 
#

library(shiny)
library(DT)
library(plotly)

nflData <- read_csv("data/nflData.csv")

nflData$win <- as.factor(nflData$win)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ############
  # Data tab #
  ############
  
  # subset the data on the data tab if the user wants any specific filters
  newData <- reactive({
    newData <- nflData %>% 
      filter(season %in% input$seasonFilter,
             team %in% input$teamFilter,
             week %in% input$weekFilter) %>%
      select(input$columnFilter)
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
                                   team %in% input$teamFilter,
                                   week %in% input$weekFilter),
                file,
                row.names = FALSE)
    }
  )
  ########################
  # Data Exploration Tab #
  ########################
  
  # histogram
  
  
  # scatter plot 
  output$scatterPlot <- renderPlotly({
    filteredData <- nflData %>%
      filter(team %in% input$teamsFilter,
             season %in% input$seasonsFilter,
             week %in% input$weeksFilter) %>%
      select(-c(week, season, team))
    
    plot <- ggplot(data = filteredData, aes_string(x = input$xVar, y = input$yVar)) +
      geom_jitter(aes(color = win)) +
      scale_color_manual(values = c("red", "black"))
    
    ggplotly(plot)
    
  })
  
  
  # the numerical summary for different variables
  output$numericSummary <- renderDT({
    
    # filter the data based on the universal filters and remove the week, season, and team columns
    filteredData <- nflData %>%
      filter(team %in% input$teamsFilter,
             season %in% input$seasonsFilter,
             week %in% input$weeksFilter) %>%
      select(-c(week, season, team)) %>%
      select(input$numVars)
    
    numericSum <- do.call(cbind, lapply(filteredData, summary))
    
    as.data.frame(t(numericSum))
  })

})

