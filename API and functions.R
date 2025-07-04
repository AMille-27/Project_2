
#API Vegan Meals Query 

library(jsonlite)
library(tidyverse)
library(httr)
Vegan_meals_query <- "https://www.themealdb.com/api/json/v1/1/filter.php?c=Vegan"
Vegan_meals_q_response <- GET(Vegan_meals_query)
Vegan_meals_q_r_parsed <-fromJSON(rawToChar(Vegan_meals_q_response$content))
Vegan_meals_q_r_p_tibble <-as_tibble(Vegan_meals_q_r_parsed)

#the above is too small in my opinion

#API for meal Category..Vegan and vegetarian = plant based vs contains meat = other
all_dietary_query <-"https://www.themealdb.com/api/json/v1/1/categories.php"
all_dietary_q_responses <- GET(all_dietary_query)
all_dietary_q_r_parsed <- fromJSON(rawToChar(all_dietary_q_responses$content))
all_dietary_q_r_p_tibble <- as_tibble(all_dietary_q_r_parsed)
all_diet_type_grouped <- all_dietary_q_r_p_tibble$categories |> 
  mutate(
    diet_type = if_else(strCategory %in% c("Vegan", "Vegetarian"), "Plant-Based","Other")
      )

#API and function for meals by region
get_meals_by_region <- function(region_name){
  url2 <- paste0("https://www.themealdb.com/api/json/v1/1/filter.php?a=", region_name)
  response <- GET(url2)
  parsed <-fromJSON(rawToChar(response$content))
  as_tibble(parsed$meals)
}

#API and function for Youtube link present or not and region
Youtube_or_not <- function(user_input, has_video = TRUE){
  url3 <- paste0("https://www.themealdb.com/api/json/v1/1/filter.php?a=", user_input)
  response <- GET(url3)
  parsed <- fromJSON(rawToChar(response$content))
  meals_area <- as_tibble(parsed$meals)
  if (has_video) {
    meals_area <- meals_area %>% filter(strYoutube !="")
  } else{
    meals_area <- meals_area %>% filter(strYoutube == "")
  }
  return(meals_area)
}
#above not working as intended ughhhhhh
