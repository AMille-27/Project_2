library("shinydashboard")
library("shiny")
library("DT")
library("httr")
library("jsonlite")
library("tidyverse")

## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Meals to Try"),
  dashboardSidebar(
    sidebarMenu( id= "tabs",
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
                  p("This app lets users explore meals from around the world 
            that they may want to try.")
                )
              ),
              tags$img(
                src = "recipe_book.png",
                height = "200px",
                style = "display: block; margin: 20px auto;"
              ),
              fluidRow(
                box(
                  title = "About the Data",
                  width = 12,
                  p("The data comes from"),
                  tags$a(href = "https://www.themealdb.com/api.php", "TheMealDB API")
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
              ),
              fluidRow(
                box(
                  title = "Choose a Category",
                  width = 12,
                  selectInput("category", "Select a category:",
                              choices = c("Beef", "Chicken", "Dessert", "Lamb", "Miscellaneous",
                                          "Pasta", "Pork", "Seafood", "Side", "Starter",
                                          "Vegan", "Vegetarian", "Breakfast", "Goat"),
                              selected = "Vegan"),
                  selectInput("area","Select a region(optional):",
                           choices = c("","American", "British", "Canadian", "Chinese", "Croatian", 
                                       "Dutch", "Egyptian", "French", "Greek", "Indian", "Irish", 
                                       "Italian", "Jamaican", "Japanese", "Kenyan", "Malaysian", 
                                       "Mexican", "Moroccan", "Polish", "Portuguese", "Russian", 
                                       "Spanish", "Thai", "Tunisian", "Turkish", "Vietnamese"),
                            selected = ""),
                sliderInput("min_ingredients", "Minimum number of ingredients:", min=1, max =20, value = 1),
                sliderInput("max_ingredients", "Maximum number of ingredients:", min =1, max= 20, value= 20),
                checkboxInput("has_video", "Only include meals with YouTube videos", value = FALSE)
                )
              ),
              fluidRow(
                box(
                  title = "Meal Data",
                  width = 12,
                  DT::dataTableOutput("filtered_data")
                )
              ),
              fluidRow(
                column(12,
                       downloadButton("download_data", "Download CSV")
                )
              )
      ),
  # Data Exploration tab content
  tabItem(tabName = "data_exploration_tab", 
        fluidRow (
          box(
            title = "Pending",
            width = 12,
            p("This will be the Data Exploration tab.")
            )
          )
       )
    )
  )
)

server <- function(input, output, session) { 

meal_data <-reactive({
  req(input$category)
  
  get_meals_all(
    category = input$category,
    min_ingredients = input$min_ingredients,
    max_ingredients = input$max_ingredients,
    area = if(input$area =="") NULL else input$area,
    has_video = input$has_video
  )
})

output$filtered_data <- DT::renderDataTable ({
  DT::datatable(meal_data())
})
output$download_data <- downloadHandler(
  filename = function() {
    paste("meals_", input$category,"_", Sys.Date(),".csv", sep ="")
  },
  content = function(file) {
    write.csv(meal_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
