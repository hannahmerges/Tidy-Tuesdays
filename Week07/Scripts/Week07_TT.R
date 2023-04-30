###### Tidy Tuesday 6 
####### Created by: Hannah Merges
###### Created on: 27-04-2023

## load libraries 
library(tidyverse)
library(here)
library(PNWColors)
library(magick)
library(janitor)
library(vtree)
library(patchwork)

#### import data  ####
netflix <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

#### tidy data to make useable for outputs #####
netflix_filtered <- netflix %>%
  select(2, 6, 8:9) %>% 
  drop_na()

tabyl(netflix_filtered, type, country) 
vtree(netflix_filtered, "type") ## this shows the total percentage of movies vs tv shows produced 

##make individual datasets for tv and movie... will later patchwork together 
netflix_tv <- netflix_filtered %>% 
  filter(type=="TV Show") %>% 
  count(country, type, sort=TRUE) %>% 
  rename(count= "n") %>% 
  mutate(country = factor(country)) %>% # make country a factor
  filter(count>22) ## keep only the countries that produced more than 10 TV shows 

netflix_movie <- netflix_filtered %>% 
  filter(type=="Movie") %>% 
  count(country, type, sort=TRUE) %>% 
  rename(count= "n") %>% 
  mutate(country = factor(country)) %>% # make country a factor
  filter(count>46) ## keep only the countries that produced more than 13 TV shows 


#### plot the data in different ways ####
## goals: what country has produced the most TV shows/movies *check*
  # ratings by country 
  # do ratings intensify over the years? 

p1 <- ggplot(data=netflix_tv, 
       aes(y=fct_reorder(country, count), 
           x=count,
           fill=country)) + 
  geom_col() +
  scale_fill_manual(values=pnw_palette("Starfish", n=14)) + 
  theme_bw() + 
  labs(x="Number of TV shows", 
       y= "Country", 
       title= "Number of TV Shows Produced by Country", 
       fill="Country") + 
  theme(plot.title=element_text(hjust=0.5), 
        axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10)) + 
  theme(legend.position = "none")
        
p1

p2 <- ggplot(data=netflix_movie, 
             aes(y=fct_reorder(country, count), 
                 x=count,
                 fill=country)) + 
  geom_col() +
  scale_fill_manual(values=pnw_palette("Starfish", n=15)) + 
  theme_bw() + 
  labs(x="Number of Movies", 
       y= "Country", 
       title= "Number of Movies Produced by Country", 
       fill="Country") + 
  theme(plot.title=element_text(hjust=0.5), 
        axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10)) + 
  theme(legend.position = "none") ## gets rid of the fill legend! 

p2

## side note: wonder how you could specifically change axis in this case to have smaller incremements at the beginning and then larger gaps between 200, 300, etc up to 600 

p1+p2 
ggsave(here("Week07", "Outputs", "productions_by_country.jpg"), 
       height=7, width=12)


#### SECOND PLOTS FOR DATA #### 
## look at the ratings per country 
## using a heat map? 

netflix_movie <- netflix_filtered %>% 
  filter(type=="Movie") %>% 
  count(country, type, sort=TRUE) %>% 
  rename(count= "n") %>% 
  mutate(country = factor(country)) %>% # make country a factor
  filter(count>46) ## keep only the countries that produced more than 13 TV shows 

netflix_movieratings <- netflix_filtered %>% 
  filter(type=="Movie") %>% 
  filter(rating=="G"|rating=="PG"|rating=="PG-13"|rating=="R"|rating=="NR") %>% 
  filter(country=="United States") %>% 
  count(country, rating, sort=TRUE) %>% 
  rename(count= "n") %>% 
  mutate(country = factor(country)) %>% # make country a factor
  filter(count>10) ## keep only the countries that produced more than 13 TV shows 

## united states had significantly more movies and ratings than compared to many other countries so just ended up focusing on them

ggplot(data = netflix_movieratings,
       aes(x=rating, 
           y=count,
           fill=rating)) + 
  geom_col() + 
  scale_fill_manual(values=pnw_palette("Starfish", n=5)) + 
  theme_bw() + 
  labs(x="Movie Ratings", 
       y="Number per Rating", 
       title="Ratings of Movies in United States", 
       fill="Rating") + 
  theme(plot.title=element_text(hjust=0.5), 
        axis.title.x = element_text(size=12), 
        axis.title.y = element_text(size=12), 
        axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10))
ggsave(here("Week07", "Outputs", "movieratings.jpg"))



