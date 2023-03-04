#############################################
### Tidy Tuesdays, Week 3 African Language Sentiment 
### Created by: Hannah Merges
### Created on: 2023-02-28
### Last updated: 2023-03-04
############################################

#################################
## Load libraries
################################
library(tidyverse)
library(here)
library(praise)
library(beyonce)
library(ggthemes)
??ggthemes()
library(devtools)
library(patchwork)
??patchwork()


##############################
## Read in Data 
##############################
AfricanLanguages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
View(AfricanLanguages)

LanguagesPerCountry <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
View(LanguagesPerCountry)

CountryRegion <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')
View(CountryRegion)

AfricanSentiment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
View(AfricanSentiment)


#########################################
## organize data and join datasets 
#########################################
Joined_Language_Country <- left_join(CountryRegion, LanguagesPerCountry)
full_language_region <- left_join(Joined_Language_Country, AfricanLanguages)
View(full_language_region)

AfricanSentiment_filetered <- AfricanSentiment %>%
  select(language_iso_code, label)
View(AfricanSentiment_filetered)

full_language_sentiment <- left_join(AfricanSentiment_filetered,full_language_region)
View(full_language_sentiment)

##okay data sets are joined --> note to self: can only join 2 at a time and remember they have to share at least one heading 

##########################################
## now work with data to make a plot 
###########################################
#how should we use the data? 
#plan A --> tweet sentiment per region and maybe use patchwork with another plot 

plot_1 <- full_language_sentiment %>%
  ggplot() + 
  geom_bar(aes(x=label,
               fill=label))+ 
  facet_wrap(~country+region, scales="free") + 
  scale_fill_manual(values = beyonce_palette(18)) + 
  coord_trans() + 
  theme_bw() + 
  labs(x="Connotation of Tweets", 
       y="Number of Tweets", 
       title="Tweet Connotation per Country and Region of Africa")
ggsave(here("Week3_AfricanLanguageSentiment", "Outputs", "tweetconnotationbarplot2.jpg"))

plot_1

##trying a second plot --> density plot: shows how much each country tweets and what the most common sentiment is
plot_2 <- full_language_sentiment %>%
ggplot(aes(x=label, 
           y=country)) +
  geom_bin2d(bins = 70) +  
  scale_fill_continuous(type = "viridis") +
  theme_bw() + 
  labs(x="Tweet Sentiment",
       y="Country")

plot_2

##it's not the most advanced or best plot BUT my goal is to figure out how to use the patchwork package 

#### sooo do that here: 
plot_1 + plot_2 %>% 
plot_annotation(caption="Two plots that illustrate the frequency of positive, negative, and neutral tweet per country in Africa")
# tried doing a caption on the plots but R was mad at me and I don't think it worked 

##omg I did it! How exciting... 
#there are things to fix about both plots, but this was the main goal for the week and reaching by R boundary time limit 

#####################################
## things for next time
#####################################
# figure out how to scale axes --> update: scale = "free" in the facet_wrap section 
# add another plot and use patchwork to bar plot 







