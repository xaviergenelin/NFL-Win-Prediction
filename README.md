# Overview

This app explores NFL data from the previous 2002-2014 seasons.

## Packages
Current packages used:
- `shiny`: app framework  
- `DT`: adds in more functionality for Shiny  
- `tidyverse`: data manipulation and visualization
- `shinyWidgets`: adds in more functionality for Shiny   
- `caret`: machine learning  
- `plotly`: interactive graphs  
- `teamcolors`: gets more information about each team such as primary colors, division, and conference  
- `jsonlite`: access an API to retrieve additional data on the teams


To install the packages run the following code:
```{r}
install.packages("shiny")
install.packages("DT")
install.packages("tidyverse")
install.packages("shinyWidgets")
install.packages("caret")
install.packages("plotly")
install.packages("teamcolors")
install.packages("jsonlite")
```

## Code to run the application 
```{r}
shiny::runGitHub("xaviergenelin/NFL-Win-Prediction", ref="main")
```