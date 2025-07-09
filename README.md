---
editor_options: 
  markdown: 
    wrap: 72
---

# Project_2

## Brief description of the app and its purpose.

This app uses TheMealDB API. This will be used to search through meals
and recipe ideas. Users will be able to search using dietary preferences
such as plantbased or other, Area, number of ingredients, number of
characters from the instructions and whether there's a youtube video.

## A list of packages needed to run the app. Users will need the
following packages: Tidyverse, JSONlite, Purrr, Shiny, ggalluvial
shinydashboard, DT, dplyr

## A line of code that would install all the packages used (so we can easily grab that and run it prior to running your app).

install.packages(c("tidyverse", "jsonlite","purrr",
"shiny","ggalluvial","shinydashboard", "DT", "dplyr)) 

## The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.
shiny::runGitHub("Project_2", "AMille-27")
