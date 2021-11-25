# Overview

This app explores NFL data from the previous 5 seasons.

## Packages
Current packages used:
- `shiny`  
- `DT`  
- `tidyverse`
- `shinyWidgets` 
- `caret`  


To install the packages run the following code:
```{r}
install.packages("shiny")
install.packages("DT")
install.packages("tidyverse")
install.packages("shinyWidgets")
install.packages("caret")
```

## Code to run the application 
```{r}
shiny::runGitHub("xaviergenelin/NFL-Win-Prediction", ref="main")
```