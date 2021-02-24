library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(corrplot)
library(RColorBrewer)

food_df <- read.csv("Food_Supply_Quantity_kg_Data.csv", header = TRUE)

#Renaming the columns
food_df <- food_df %>% rename(., Alcohol = Alcoholic.Beverages, Animal_fats = Animal.fats, Animal_products = Animal.Products, 
                              Aquatic_product = Aquatic.Products..Other, Cereals = Cereals...Excluding.Beer, Fish_seafood = Fish..Seafood,
                              Fruits = Fruits...Excluding.Wine, Milk = Milk...Excluding.Butter, Starchy_roots = Starchy.Roots, Sugar_sweetners = Sugar...Sweeteners,
                              Sugar_crops = Sugar.Crops, Vegetable_oils = Vegetable.Oils, Vegetal = Vegetal.Products)

#Get the corresponding continent name
food_df$Continent <- countrycode(sourcevar = food_df[, "Country"],
                                 origin = "country.name",
                                 destination = "continent")

#selecting the requierd columns
food <- food_df %>% select(.,Country, Continent, Alcohol, Animal_products, Cereals, Fruits, Milk, Pulses,Vegetable_oils,Vegetables,Vegetal,
                           Obesity, Undernourished,Confirmed, Deaths, Recovered) 

#Changingthe America into south america and north america
food <- food%>% mutate(., Continent = (ifelse (Country == "United States of America", "North America",Continent))) %>% 
  mutate( Continent = (ifelse(Country == "Canada", "North America",Continent)))
food <- food %>% mutate(., Continent = gsub("Americas", "South America", Continent))       

#Removing the NAs
food<- food[complete.cases(food), ]

#Creating Dataframe for the food profile for all countries by filtering out non-requiered columns and removing NAs

food_country <- food_df %>% select(-c(Miscellaneous, Vegetal,Undernourished, Deaths, Active, Recovered, Population, Unit..all.except.Population., Continent))
food_country <- food_country[complete.cases(food_country), ]

#Creating a new column obesity level if the obesity is greater than the avgerage Obesity
food_obes <- food %>% mutate(obesity_level = ifelse (Obesity > mean(Obesity), "High", "Low"))

# first convert the Undernorished column into numerical vector. The string "<2.5" was converted to number 2. 
#Creating a new column UN_rate (less, middle and high) according to the Undernorishment factors

food_UN <- food %>% mutate(Undernourished = ifelse(Undernourished == "<2.5", 2.0, Undernourished)) %>% 
  mutate(UN_rate = (ifelse(Undernourished <=2, "Lower than 2.5", ifelse((Undernourished > 2 & Undernourished < 15), "Middle(2-15)", "High(15 & above)"))))  

#Creating the correlation matrix for Animal_products, Fruits, Vegetables,Obesity, Undernourished,Confirmed, Deaths variables
food1 <- food %>% select(., Animal_products, Fruits, Vegetables,Obesity, Undernourished,Confirmed, Deaths) %>% mutate(Undernourished = ifelse(Undernourished == "<2.5", 2.0, Undernourished)) 
food_corr<- round(cor(food1), 2)

