
# Find Data You Are Interested In

library(jsonlite)
library(tidyverse)
library(purrr)
library(dplyr)
library(httr)
library(ggalluvial) 
#API Vegan Meals Query 

Vegan_meals_query <- "https://www.themealdb.com/api/json/v1/1/filter.php?c=Vegan"
Vegan_meals_q_response <- GET(Vegan_meals_query)
Vegan_meals_q_r_parsed <-fromJSON(rawToChar(Vegan_meals_q_response$content))
Vegan_meals_q_r_p_tibble <-as_tibble(Vegan_meals_q_r_parsed)

#the above is too small in my opinion

## API for meal Category..Vegan and vegetarian = plant based vs contains meat = other
all_dietary_query <-"https://www.themealdb.com/api/json/v1/1/categories.php"
all_dietary_q_responses <- GET(all_dietary_query)
all_dietary_q_r_parsed <- fromJSON(rawToChar(all_dietary_q_responses$content))
all_dietary_q_r_p_tibble <- as_tibble(all_dietary_q_r_parsed)
all_diet_type_grouped <- all_dietary_q_r_p_tibble$categories |> 
  mutate(
    diet_type = if_else(strCategory %in% c("Vegan", "Vegetarian"), "Plant-Based","Other")
      )

## API and function for meals by region
get_meals_by_region <- function(region_name){
  url2 <- paste("https://www.themealdb.com/api/json/v1/1/filter.php?a=", region_name, sep = "")
  response <- GET(url2)
  parsed <-fromJSON(rawToChar(response$content))
  as_tibble(parsed$meals)
}

#going to another query
## Meals by category
all_dietary_query <- "https://www.themealdb.com/api/json/v1/1/categories.php"
all_dietary_response <- GET(all_dietary_query)
all_dietary_parsed <- fromJSON(rawToChar(all_dietary_response$content))
all_categories <- as_tibble(all_dietary_parsed$categories)

get_meals_by_category <-function(category){
  url4 <-paste("https://www.themealdb.com/api/json/v1/1/filter.php?c=", category,sep = "")
  response <- GET(url4)
  parsed <- fromJSON(rawToChar(response$content))
  all_df <-as_tibble(parsed$meals)
  all_df$diet_type <- category 
  return(all_df)
}

## meals by Number of ingredients

all_categories_cv <- c("Beef","Chicken","Dessert","Lamb","Miscellaneous",
      "Pasta" ,"Pork","Seafood" ,"Side","Starter", "Vegan","Vegetarian",
      "Breakfast","Goat")
all_meals_df <- purrr::map_dfr(all_categories_cv, get_meals_by_category)

get_meals_by_id <-function(id){
  url5 <-paste("https://www.themealdb.com/api/json/v1/1/lookup.php?i=", id, sep ="")
  response <- GET(url5)
  parsed <- fromJSON(rawToChar(response$content))
  all_id <- as_tibble(parsed$meals)
  return(all_id)
}

meals_ids <- all_meals_df$idMeal
full_all_meals <- purrr::map_dfr(meals_ids, get_meals_by_id)

### actual count
full_all_meals_longer <- pivot_longer(
  full_all_meals, 
  cols = starts_with("strIngredient"),
  names_to = "Ingredient_names",
  values_to = "ingredient"
) |>
  filter(!is.na(ingredient) & ingredient != "")

full_all_meals_longer_select <-
  full_all_meals_longer|>select(idMeal, Ingredient_names, ingredient)
count_all_meals_ingredients <- full_all_meals_longer_select |> group_by(idMeal) |> summarize(number_ingredients = n())

full_all_meals <- full_all_meals |>
  left_join(count_all_meals_ingredients, by = "idMeal")
 
  
## API and function for Youtube link present 
Youtube <- function(data, has_video) {
  if (has_video) {
    data |> 
      dplyr::filter(!is.na(.data$strYoutube) & nchar(.data$strYoutube) > 0)
  } else {
    data |>
      dplyr::filter(is.na(.data$strYoutube) | nchar(.data$strYoutube) == 0)
  }
}
#Do I need to go back to add a yes or no column to the table or make a new tibble??

## Recipe Meal Name length 
full_all_meals <- full_all_meals |>
  mutate(meal_name_length = nchar(strMeal))

## Recipe Instruction Length
full_all_meals <- full_all_meals |>
  mutate(instruction_length = nchar(strInstructions))

## adding everything to one tibble to make summarize easier
full_all_meals <-full_all_meals |> 
  mutate(diet_type = if_else(strCategory %in% c("Vegan", "Vegetarian"), "Plant-Based","Other"),
         has_youtube = if_else(!is.na(strYoutube) & nchar(strYoutube) > 0, "Yes", "No") )

# Know How to Summarize the Data

## Video Availability by Area  stacked bar graph
full_all_meals |>
  count(strArea, has_youtube) |>
  ggplot(aes(fill= has_youtube, y= n, x = strArea))+
  geom_bar(position = "stack", stat= "identity") +
  coord_flip() +
  geom_text(aes(label =n))+
  labs(title = "Video Availability by Area", y = "Number of meals", x = "Region")
  
## Plant Based or other by stacked bar graph
 full_all_meals |>
   count(diet_type, strArea) |>
   ggplot(aes(fill = diet_type, y = n, x = strArea)) +
   geom_bar(position = "stack", stat = "identity")+
   coord_flip() +
   geom_text(aes(label =n))+
   labs(title = "Plant-Based Vs Other by Area", y = "Number of meals", x = "Region")

 ## Contingency table "Number of ingredients" and Youtube videos 
 # need to categorize complexity based on number of ingredients. 
 summary(full_all_meals$number_ingredients, na.rm = TRUE)
 # if meal has less than 8 ingredients few, be 8 and 13 moderate and > 13 Many
#adding a column with the categories 
 full_all_meals <- full_all_meals|>
   mutate(ingredient_complexity = case_when(
     number_ingredients < 8 ~ "Few",
     number_ingredients >=8 & number_ingredients <= 13 ~"Moderate",
     number_ingredients > 13 ~ "Many"
   ))
#adding the contingency table 
 full_all_meals |> 
   group_by (ingredient_complexity, has_youtube) |>
   summarize(n=n()) |>
   pivot_wider(
     names_from = has_youtube,
     values_from = n
   )
   
#Box Plot of Ingredients and Area
 full_all_meals |>
   ggplot(aes(x= strArea , y= number_ingredients)) +
   geom_boxplot(fill = "white", colour = "#3366FF")+
   coord_flip()+
   labs(title = "Regional Comparion of Number of Ingredients", 
        y = "Number of ingredients", x = "Region")
 
#table of the above information
  summarystats_region_vs_numIngredients<- full_all_meals |>
   group_by(strArea) |>
   summarize(
     min_ingredients = min(number_ingredients),
     Q1_ingredients = quantile(number_ingredients,.25),
     median_ingredients = median(number_ingredients),
     Q3_ingredients = quantile(number_ingredients,.75),
     mean_ingredients = mean(number_ingredients),
     max_ingredients = max(number_ingredients),
     sd_ingredients = sd(number_ingredients))
    

#creative plot alluvial plot 
library(ggalluvial)  
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
#histogram
ggplot(data = full_all_meals , aes(x = instruction_length, fill = ingredient_complexity))+
  geom_histogram() +
  labs(title = "Instruction Length Distribution", x= "number of characters", 
       )

#scatterplot
ggplot(data=full_all_meals, aes(x= number_ingredients, y= instruction_length,
                    color = strCategory))+
  geom_point()+
  geom_jitter()+
  labs(title = "Number of Ingredients Vs Instruction Length", 
       x= "number of Ingredients", y= "Number of Characters in Instruction")

#cleaning up to have my get fn combined to make app look better
get_meals_all <- function (category = NULL,
                           min_ingredients = NULL,
                           max_ingredients = NULL,
                           area = NULL,
                           has_video = NULL) {
  
  base_df <- get_meals_by_category(category)
  ids <- base_df$idMeal
  
  full_meals <- purrr::map_dfr(ids, get_meals_by_id)
  
  #number of ingredients prework
  counts_ingredient <-full_meals |>
    select(idMeal, starts_with("strIngredient")) |>
    pivot_longer(cols= starts_with("strIngredient"), 
                 names_to = "ingredient_name",
                 values_to = "ingredient") |>
    filter(!is.na(ingredient) & ingredient != "") |>
    count(idMeal, name= "number_ingredients")
  
  full_meals <- full_meals |> #putting it back together
    left_join(counts_ingredient, by = "idMeal")
  
  if (!is.null(min_ingredients)) {
    full_meals <- dplyr::filter(full_meals, number_ingredients >= min_ingredients)
  }
  if (!is.null(max_ingredients)){
    full_meals <-dplyr::filter(full_meals, number_ingredients <= max_ingredients)
  }
  
  if (!is.null(area)) {#by Area
    full_meals <- dplyr::filter(full_meals, strArea == area)
  }
  
  if (!is.null(has_video) && has_video ==TRUE) { #youtube or not
    full_meals <- Youtube(full_meals, has_video)
  }
  
  return(full_meals)
   }