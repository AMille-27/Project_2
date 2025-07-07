
# Find Data You Are Interested In

library(jsonlite)
library(tidyverse)
library(purrr)
library(httr)
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

## Video Availability by Area  
full_all_meals |>
  count(strArea, has_youtube) |>
  ggplot(aes(fill= has_youtube, y= n, x = strArea))+
  geom_bar(position = "stack", stat= "identity") +
  coord_flip() +
  geom_text(aes(label =n))+
  labs(title = "Video Availability by Area", y = "Number of meals", x = "Region")
  
## Plant Based or other by region contingency table
 full_all_meals |>
   count(diet_type, strArea) |>
   ggplot(aes(fill = diet_type, y = n, x = strArea)) +
   geom_bar(position = "stack", stat = "identity")+
   coord_flip() +
   geom_text(aes(label =n))+
   labs(title = "Plant-Based Vs Other by Area", y = "Number of meals", x = "Region")

 ## Contingency table "Number of ingredients" and Youtube videos 
 # need to categorize complexity based on number of ingredients. 
 
 

