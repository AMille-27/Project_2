library("shinydashboard")
library("shiny")

## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Meals to Try"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about_tab"),
      menuItem("Data Download", tabName = "data_download"),
      menuItem("Data Exploration", tabName = "data_exploration_tab")
    )
  ),
  dashboardBody(
    tabItems(
      # About tab content
      tabItem(tabName = "about_tab",
              fluidRow(
                box(
                  title = "App Purpose",
                  width = 12,
                  p("This app let's users explore meals from around the world 
                    that they may want to try.")
                )
              ),
              fluidRow(
                box(
                  title = "About the Data",
                  width = 12,
                  p("The data comes from"),
                  tags$a(href= "https://www.themealdb.com/api.php", "TheMealDB API")
                 )
              ),
              fluidRow(
                box(
                  title= "Image",
                  width = 12,
                  tags$img(src = "Recipe_book.png", height = "200px" )
                )
              )
      ),
      
      # Data Download tab content
      tabItem(tabName = "data_download",
            fluidRow(
              box(
                title = "Download Setup",
                width = 12,
                p("This is the tab where users select which data to download.")
              )
          )
      ),
      # Data Exploration tab content
      tabItem(tabName = "data_exploration_tab")
           )
       )
  )

server <- function(input, output) { }

shinyApp(ui, server)

box(plotOutput("plot1", height = 250)),

box(
  title = "Controls",
  sliderInput("slider", "Number of observations:", 1, 100, 50)