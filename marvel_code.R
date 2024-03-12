library(tidyverse)
library(ggplot2)

marvel_data <-read_csv("Data/marvel_box_office.csv")
spec(marvel_data)
library(janitor)

marvel_data <- clean_names(marvel_data)

glimpse(marvel_data)
View(marvel_data)

#number of movies each year
marvel_data |>
  count(release_year, sort = TRUE)

ggplot(data = marvel_data)+
  geom_bar(mapping = aes(x = release_year))


ggplot(marvel_data, aes(x = release_year))+
  geom_bar(fill = "black")
marvel_data |>
  ggplot(aes(x = release_year, fill = ownership))+
  geom_bar()

#number of movies released in each month
marvel_data |>
  ggplot(aes(x = release_month))+
  geom_bar(aes(fill = ownership))
marvel_data |>
  count(release_month, sort = TRUE)


#number of movies released in each ownership
marvel_data |>
  ggplot(aes(x = ownership))+
  geom_bar(aes(fill = ownership))

#number of movies released in each phase
ggplot(marvel_data, aes(x = phase))+
  geom_bar(aes(fill = phase))

# total budget for movies in each phase and its world wide box office collection
marvel_data |>
  group_by(phase)|>
  summarise(total_budget = sum(budget),
            total_collction_worldwide = sum(worldwide_box_office))|>
  ggplot(aes(x = phase, y = total_budget))+
  geom_col(aes(fill = phase))+
  geom_point(aes(y = total_collction_worldwide, color = phase))

View(marvel_data)


# total box office collecton and budget in each year.
library(dplyr)

mcu_collect <- marvel_data |>
  group_by(release_year, ownership) |>
  summarise(across(c(domestic_box_office, inflation_adjusted_domestic, 
              international_box_office, inflation_adjusted_international, 
              worldwide_box_office, inflation_adjusted_worldwide, 
              opening_weekend, inflation_adjusted_opening_weekend, 
              budget, inflation_adjusted_budget),sum),
            .groups = 'drop')

#Domestic box office collection in each year for every ownership company
mcu_collect |>
  ggplot(aes(x = release_year, y = domestic_box_office))+
  geom_line(color = "green")+
  facet_wrap(~ownership)

#domestic, international, world wide and opening weekend box office collecton 
#in each year for every company
mcu_collect |>
  ggplot(aes(x = release_year))+
  geom_line(aes(y = domestic_box_office),color = "green")+
  geom_line(aes(y = international_box_office), color = "red")+
  geom_line(aes(y = worldwide_box_office), color = "black")+
  geom_line(aes(y = opening_weekend), color = "blue")+
  facet_wrap(~ownership)

mcu_collect |>
  ggplot(aes(x = release_year))+
  geom_line(aes(y = domestic_box_office),color = "green")+
  geom_line(aes(y = inflation_adjusted_domestic), color = "green")+
  geom_line(aes(y = international_box_office), color = "red")+
  geom_line(aes(y = inflation_adjusted_international), color = "red")+
  geom_line(aes(y = worldwide_box_office), color = "black")+
  geom_line(aes(y = inflation_adjusted_worldwide), color = "black")+
  geom_line(aes(y = opening_weekend), color = "blue")+
  geom_line(aes(y = inflation_adjusted_opening_weekend), color = "blue")+
  facet_wrap(~ownership)

View(mcu_collect)

#movie which have highest and lowest budget

marvel_data[which.max(marvel_data$budget),]
marvel_data[which.min(marvel_data$budget),]

marvel_data |>
  slice_max(budget)
marvel_data |>
  slice_min(budget)
  

#movie which have highest and lowest international collection in each year
marvel_max <- marvel_data |>
  group_by(release_year) |>
  slice_max(international_box_office) |>
  select(movie, release_year, international_box_office, budget)|>
  arrange(release_year)

marvel_min <- marvel_data |>
  group_by(release_year)|>
  slice_min(international_box_office) |>
  select(movie, release_year, international_box_office, budget)

ggplot()+
  geom_col(data = marvel_max,
           aes(x = release_year, y = international_box_office), fill = "blue")+
  geom_col(data = marvel_min, 
           aes(x = release_year, y = international_box_office), fill = "orange")


#relationship between budget and worldwide collection of each movies
 marvel_data |>
   arrange(release_year) |>
   ggplot(aes(x = budget, y = worldwide_box_office, color = release_year))+
   geom_point(position = "jitter")+
   facet_wrap(~phase)

 #each box office collection Vs imdb score

 marvel_data |>
   ggplot(aes(x = im_db_score, y = worldwide_box_office))+
   geom_point(position = "jitter")+
   geom_smooth()+
   facet_wrap(~ownership)
 
 
 marvel_data |>
   ggplot(aes(x = im_db_score, y = international_box_office))+
   geom_point(position = "jitter")+
   geom_smooth()+
   facet_wrap(~ownership)

 marvel_data |>
   ggplot(aes(x = im_db_score, y = budget))+
   geom_point(position = "jitter")+
   geom_smooth()+
   facet_wrap(~ownership)
 
 # number of movies which is higher and lower of each ratings
 
 imdb_mean <- mean(marvel_data$im_db_score)
 meta_mean <- mean(marvel_data$meta_score)
 tomato_mean <- mean(marvel_data$tomatometer)
 rotten_mean <- mean(marvel_data$rotten_tomato_audience_score)
 

 library(tidyverse)
 
 marvel_ratings <- marvel_data |>
   mutate(imdb_rate = ifelse(im_db_score > imdb_mean, "above_mean", "below_mean"))|>
   mutate(meta_rate = ifelse(meta_score > meta_mean, "above_mean","below_mean"))|>
   mutate(tomato_rate = ifelse(tomatometer > tomato_mean, "above_mean","below_mean"))|>
   mutate(rotten_rate = ifelse(rotten_tomato_audience_score > rotten_mean, "above_mean","below_mean"))
 
 View(marvel_ratings)
 
 marvel_ratings |>
   select(movie, release_year,imdb_rate,meta_rate, tomato_rate, rotten_rate)|>
   pivot_longer(
     cols = imdb_rate:rotten_rate,
     names_to = "all_ratings",
     values_to = "above_or_below"
   ) |>
   ggplot(aes(x = above_or_below))+
   geom_bar(aes(fill = all_ratings))

  library(dplyr)
 marvel_ratings |>
   select(movie, release_year, ownership,ends_with("rate"),
          ends_with("box_office"))|>
   pivot_longer(
     cols = ends_with("box_office"),
     names_to = "box_office_type",
     values_to = "collection"
   ) |>
   pivot_longer(
     cols = imdb_rate:rotten_rate,
     names_to = "all_ratings",
     values_to = "above_or_below"
   )|>
   ggplot(aes(x = release_year, y = collection))+
   geom_line(position = "jitter", color ="red")+
   facet_grid(box_office_type~all_ratings)
 
 
 marvel_ratings |>
   select(movie, release_year, ownership,ends_with("rate"),
          ends_with("box_office"))|>
   pivot_longer(
     cols = ends_with("box_office"),
     names_to = "box_office_type",
     values_to = "collection"
   ) |>
   pivot_longer(
     cols = imdb_rate:rotten_rate,
     names_to = "all_ratings",
     values_to = "above_or_below"
   )|>
   ggplot(aes(x = release_year, y = collection))+
   geom_line(aes(color = ownership),position = "jitter")+
   facet_grid(box_office_type~all_ratings)+
   theme(axis.text.x = element_text(angle = 45))
 
 

 #above mean for every ratings
 
 marvel_ratings |>
   select(movie, release_year, ownership,ends_with("rate"),
          ends_with("box_office"))|>
   pivot_longer(
     cols = ends_with("box_office"),
     names_to = "box_office_type",
     values_to = "collection"
   ) |>
   pivot_longer(
     cols = imdb_rate:rotten_rate,
     names_to = "all_ratings",
     values_to = "above_or_below"
   )|>
   ggplot(aes(x = release_year, y = collection))+
   geom_line(aes(color = above_or_below), position = "jitter")+
   facet_grid(box_office_type~all_ratings)+
   theme(axis.text.x = element_text(angle = 45))
   