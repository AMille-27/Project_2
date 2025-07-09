library("shinydashboard")
library("shiny")
library("DT")
library("httr")
library("jsonlite")
library("tidyverse")
library(ggalluvial)

## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(skin = "green",
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
                ) #ends App purpose box
              ), #ends fluid Row
              tags$img(
                src = "recipe_book.png",
                height = "200px",
                style = "display: block; margin: 20px auto;"
              ), #ends picture recipe_book
              fluidRow(
                box(
                  title = "About the Data",
                  width = 12,
                  p("The data comes from"),
                  tags$a(href = "https://www.themealdb.com/api.php", "TheMealDB API")
                ) #ends about the data box
              ) #ends the about the data fluid Row
      ), #ends the about tab content
      
      # Data Download tab content
      tabItem(tabName = "data_download",
              fluidRow(
                box(
                  title = "Download Setup",
                  width = 12,
                  p("This is the tab where users select which data to download.")
                ) #ends download set up box
              ), #ends download fluidrow
              fluidRow(
                box(
                  title = "Choose a Category",
                  width = 12,
                  selectInput("category", "Select a category:",
                              choices = c("Beef", "Chicken", "Dessert", "Lamb", "Miscellaneous",
                                          "Pasta", "Pork", "Seafood", "Side", "Starter",
                                          "Vegan", "Vegetarian", "Breakfast", "Goat"),
                              selected = ""),
                  selectInput("area","Select a region(optional):",
                           choices = c("","American", "British", "Canadian", "Chinese", "Croatian", 
                                       "Dutch", "Egyptian", "French", "Greek", "Indian", "Irish", 
                                       "Italian", "Jamaican", "Japanese", "Kenyan", "Malaysian", 
                                       "Mexican", "Moroccan", "Polish", "Portuguese", "Russian", 
                                       "Spanish", "Thai", "Tunisian", "Turkish", "Vietnamese"),
                            selected = ""),
                sliderInput("min_ingredients", "Minimum number of ingredients:", min=1, max =20, value = 1),
                sliderInput("max_ingredients", "Maximum number of ingredients:", min =1, max= 20, value= 20),
                checkboxInput("has_video", "Only include meals with YouTube videos", value = FALSE),
                selectInput("ingredient_complexity","Select a level(optional):",
                            choices = c("Few","Moderate","Many", ""),
                            selected = "")
                ) #ends choose a category box
              ), #ends choose a category fluid row
              fluidRow(
                box(
                  title = "Meal Data",
                  width = 12,
                  DT::dataTableOutput("filtered_data")
                ) #ends "Meal Data Box
              ), #ends Meal Data fluid row
              fluidRow(
                column(12,
                       downloadButton("download_data", "Download CSV")
                ) #ends column command
              ) #ends fluidrow column
      ), #ends download tab content
  # Data Exploration tab content
  tabItem(tabName = "data_exploration_tab", 
        fluidRow (
          box(
            title = "Description",
            width = 12,
            p("This will be the Data Exploration tab.")
            ) # ends description box
          ), #ends description fluidRow
        fluidRow(
          box(
            title = "Exploration",
            width = 12,
            selectInput("column_choice","Choose a column to Explore:",
                        choices = c("strArea", "number_ingredients",
                                    "ingredient_complexity", "instruction_length", "has_video"),
                        selected = "strCategory"),
            selectInput("Plot_type","Choose a Plot type:",
                        choices = c("Bar Plot", "Histogram","BoxPlot"),
                        selected = "Bar Plot"),
            plotOutput("column_plot"),
            tableOutput("column_summary"),
          )
        ),
        fluidRow(
          box(
            title = "First Plot",
            width = 12, plotOutput("histogram")
            )#ends box of first plot histogram
        ), #ends fluid Row of first plot histogram
        
        fluidRow(
          box(
            title = "Second Plot",
            width = 12, plotOutput("boxplot1")
          )#ends box of second plot boxplot1
        ), #ends fluid Row of second plot boxplot1
        
        fluidRow(
          box(
            title = "Third Plot",
            width = 12, 
            selectInput(
              inputId = "selected_Category",
              label = "Select a Meal Category:",
              choices = c("All", "Beef", "Chicken", "Dessert", "Lamb", "Miscellaneous",
                          "Pasta", "Pork", "Seafood", "Side", "Starter",
                          "Vegan", "Vegetarian", "Breakfast", "Goat"),,
              selected = "All"
            ),
            plotOutput("scatterplot1")
            )#ends box of third plot 
        ), #ends fluid Row of third plot 
        fluidRow(
          box(
            title = "Fourth Plot",
            width = 12, plotOutput("stackedbar1")
          )#ends box of fourth plot 
        ), #ends fluid Row of fourth plot 
        fluidRow(
          box(
            title = "Fifth Plot",
            width = 12, plotOutput("stackedbar2")
          )#ends box of fifth plot 
        ), #ends fluid Row of fifth plot 
        
        fluidRow(
          box(
            title = "Sixth",
            width = 12, plotOutput("ggalluvial")
          )#ends box of sixth plot 
        ) #ends fluid Row of sixth plot 
       ) #ends tabItem "data_exploraration"
    ) #ends tabItem all tabs
  ) #ends dashboard body
) #ends dashboardpage

server <- function(input, output, session) { 

meal_data <-reactive({
  req(input$category)
  
  get_meals_all(
    category = input$category,
    min_ingredients = input$min_ingredients,
    max_ingredients = input$max_ingredients,
    ingredient_complexity=input$ingredient_complexity,
    area = if(input$area =="") NULL else input$area,
    has_video = input$has_video
  )
})

output$column_plot <- renderPlot({
  data <- meal_data()
  col <- input$column_choice
  plot_type <- input$plot_type
  
  if(is.null(data[[col]]) || all(is.na(data[[col]]))) {
    return(
      ggplot() +
      annotate("text", x=0.5, y=0.5, 
               label= "Column not available or all NA values") +
             theme_void()
      )
  }
  
if(!(col %in% names(data))) {#verfying number and t/f
  return(
    ggplot()+
    annotate("text", x=0.5, y= 0.5,
             label = paste("Column", col, "not found")) +
      theme_void()
  )
}
if(all(is.na(data[[col]]))) {
  return(
    ggplot()+
    annotate("text", x=0.5, y= 0.5,
             label= paste("Column", col, "has only NA values")) +
      theme_void()
  )
}
  is_numeric <- isTRUE(is.numeric(data[[col]]))
  
  if (plot_type == "Bar Plot" && !is_numeric) {
  
  ggplot(data, aes_string(x = col)) +
    geom_bar(fill = "green") +
    labs(title = paste("Bar Plot of", col),
         x = col, y = "Count") +
      theme_minimal()
    
} else if (plot_type == "Histogram" && is_numeric) {
  ggplot (data, aes_string(x= col)) +
    geom_histogram(fill= "blue", bins=20, color= "white") +
    labs(title = paste("Histogram of", col), 
         x = col, y = "Frequency") +
    theme_minimal()
  
} 
  else if (plot_type == "Boxplot" && is_numeric) {
    ggplot(data, aes_string(y= col)) +
    geom_boxplot(fill ="gray") +
    labs(title = paste("Boxplot of", col),
         y = col) +
      theme_minimal()
    
} else {
  ggplot()+
    annotate("text", x = 0.5, y = 0.5, 
             label = "Incompatible plot type for this column", size = 6) +
    theme_void()
  }
})

output$column_summary <- renderTable({
  data <- meal_data()
  col <- input$column_choice
  
  data |>
    count(.data[[col]], name = "Count") |>
    arrange(desc(Count))
})

output$filtered_data <- DT::renderDataTable ({
  DT::datatable(meal_data())
})

output$histogram <- renderPlot ({
  #histogram
  ggplot(data = full_all_meals , aes(x = instruction_length, fill = ingredient_complexity))+
    geom_histogram() +
    labs(title = "Instruction Length Distribution", x= "number of characters", y= "frequency" 
    )
})
output$boxplot1 <- renderPlot ({
  #boxplot
  full_all_meals |>
    ggplot(aes(x= strArea , y= number_ingredients)) +
    geom_boxplot(fill = "white", colour = "#3366FF")+
    coord_flip()+
    labs(title = "Regional Comparion of Number of Ingredients", 
         y = "Number of ingredients", x = "Region")
})

output$scatterplot1 <-renderPlot ({
  #scatter plot
  data <- full_all_meals
  if (input$selected_Category != "All") {
    data <- data |> filter(strCategory == input$selected_Category)
  } 
  ggplot(data , aes(x= number_ingredients, y= instruction_length,
                                  color = strCategory))+
    geom_point()+
    geom_jitter()+
    labs(title = "Number of Ingredients Vs Instruction Length", 
         x= "number of Ingredients", y= "Number of Characters in Instruction")
})

output$stackedbar1 <-renderPlot ({
  ## Video Availability by Area  stacked bar graph
  full_all_meals |>
    count(strArea, has_youtube) |>
    ggplot(aes(fill= has_youtube, y= n, x = strArea))+
    geom_bar(position = "stack", stat= "identity") +
    coord_flip() +
    geom_text(aes(label =n))+
    labs(title = "Video Availability by Area", y = "Number of meals", x = "Region")
})
output$stackedbar2 <-renderPlot({
  ## Plant Based or other by stacked bar graph
  full_all_meals |>
    count(diet_type, strArea) |>
    ggplot(aes(fill = diet_type, y = n, x = strArea)) +
    geom_bar(position = "stack", stat = "identity")+
    coord_flip() +
    geom_text(aes(label =n))+
    labs(title = "Plant-Based Vs Other by Area", y = "Number of meals", x = "Region")
})
output$ggalluvial <-renderPlot({
  touse_ggalluvial <- full_all_meals |>
    count(strCategory, strArea, ingredient_complexity, has_youtube)
  #to make it so my table isn't so jumble up, got get the numbers per area,
  #get a list of the small, add them together to make an others, and then add them
  #to tibble to be able to use the galluvial but not jumbled 
  area_counts <- touse_ggalluvial |> 
    count(strArea, wt = n)
  category_counts <- touse_ggalluvial |>
    count(strCategory, wt = n)
  small_areas <- area_counts |> 
    filter(n < 10) |> 
    pull(strArea)
  small_categories <- category_counts |> 
    filter(n < 5) |> 
    pull(strCategory)
  touse_ggalluvial <- touse_ggalluvial |>
    mutate(
      strArea = if_else(strArea %in% small_areas, "Rare Regions", strArea),
      strCategory = if_else(strCategory %in% small_categories| strCategory =="Miscellaneous", 
                            "Miscellaneous", strCategory)
    )
  touse_ggalluvial <- touse_ggalluvial |>
    mutate(
      strArea = factor(strArea),
      strCategory = factor(strCategory)
    )
  ggplot( data = touse_ggalluvial,
          aes(axis1 = strCategory, axis2 = strArea, axis3= ingredient_complexity,
              y = n)) +
    scale_x_discrete(limits = c("Category","Area","ingredient_complexity"), 
                     expand = c(.2,.05)) + 
    xlab("Meal Information")+
    ylab("Number of Meals")+
    geom_alluvium(aes(fill = has_youtube)) +
    geom_stratum() + 
    geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    ggtitle("Meals on MealDB by Category, Area and Complexity",
            "stratified by YouTube video availability")
})
output$download_data <- downloadHandler(
  filename = function() {
    paste("meals_", input$category,"_", Sys.Date(),".csv", sep ="")
  },
  content = function(file) {
    write.csv(meal_data(), file, row.names = FALSE)
    } #closes content function
  ) #ends output$download_data
} #ends server function

shinyApp(ui, server)
