#
# author: Xavier Genelin
# date: 11/22/2021
# purpose: the back end code to explore nfl game data and predict a winner 
#

library(shiny)
library(DT)
library(plotly)

# load in the team data
teamData <- read_csv("data/teamData.csv")

teamData$win <- as.factor(teamData$win)
teamData$week <- as.numeric(teamData$week)
teamData$date <- as.Date(teamData$date)

# game data for the visuals
gameVisualData <- read_csv("data/visualGameData.csv")

# dataset of the schedule
schedule <- read_csv("data/schedule.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  ############
  # Data tab #
  ############
  
  # subset the data on the data tab if the user wants any specific filters
  newData <- reactive({
    # give user the chance to arrange by season and week/team?
    ### breks if season column isn't selected in the output, and week/team also
    newData <- teamData %>% 
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
      write.csv(teamData %>% filter(season %in% input$seasonFilter,
                                   team %in% input$teamFilter,
                                   week %in% input$weekFilter),
                file,
                row.names = FALSE)
    }
  )
  ########################
  # Data Exploration Tab #
  ########################
  
  ### Team
  
  # data based on user inputs
  graphDataTeam <- reactive({
    teamData %>%
      filter(team %in% input$teamsFilterTeam,
             season %in% input$seasonsFilterTeam,
             week %in% input$weeksFilterTeam)
  })
  
  
  # line graph
  output$lineGraphTeam <- renderPlotly({
    
    linePlot <- ggplot(data = graphDataTeam(), aes(x = date, group = team, color = team)) +
      geom_line(aes_string(y = input$lineVarTeam))
    
    ggplotly(linePlot, tooltip = c("x", "y", "group"))
  })

  
  # scatter plot of team data
  output$scatterPlotTeam <- renderPlotly({

    plot <- ggplot(data = graphDataTeam(), aes_string(x = input$xVarTeam, y = input$yVarTeam)) +
      geom_jitter(aes(color = win)) +
      scale_color_manual(values = c("red", "black"))
    
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
  
  #####################
  # Data Modeling Tab #
  #####################
  
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
  
  output$trainWeeksInput <- renderUI({
    
    numericInput(
      inputId = "trainWeeks",
      label = "Select the number of weeks to use as training",
      value = input$weekModel - 1,
      # use 2 weeks of training to ensure that a team will have data if they select a time during a team's bye week
      # if they want to predict for week 2 then they can only use week 1 data for training
      min = ifelse(input$weekModel == 2, 1, 2), 
      max = input$weekModel - 1,
      step = 1
    )
  })

})

