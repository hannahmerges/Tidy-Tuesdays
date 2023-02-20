#############################################
### Tidy Tuesdays, Week 1 
### Created by: Hannah Merges
### Created on: 2023-02-15
### Last updated: 2023-02-19

##########################
#### Load libraries 
###########################
library(tidyverse)
library(here)
library(praise)
library(devtools)
library(beyonce)
library(ggthemes)
library(calecopal)
library(ggplot2)
library(hrbrthemes)
library(readr)
library(ggforce)
library(ggcorrplot)
library(plotly)

#############################
## theme of this week's tidy tuesday -- Hollywood age gaps between love interests 
###########################
### Read in Data 
### need the readr package to take data/csv from github and use it in R. 
urlfile="https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv"

agegaps <- read_csv(url(urlfile))
View(agegaps)

#yay! I figured out how to do this, get praised :) 
praise()

#### okay now time to work with the data and create a visual 
##try a new plot, found a correlation matrix plot on ggplot extensions
## could be interesting to see if there is a correlation with character_1 gender and age gap?? OR age gap and release date
## have movies decreased the age gaps between actors over the years? 
## correlation needs to have both axes as continuous vars so this wouldn't work 

devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)

# Compute a correlation matrix
#want to filter out the data though 
#agegaps_filtered <- agegaps %>% 
# drop_na() %>% ##filters out everything that is not a complete row 
#filter(age_difference>9)

#need to make a new dataframe to focus only on 2 variables I am investigating 
#agegap2 <- agegaps[,c(2,4)] ##keeps only columns 2 and 4
#View(agegap2)

## now filter out and drop NAs 
#agegap2_filtered <- agegap2 %>% 
# filter(age_difference>9) %>% 
# drop_na() 
#View(agegap2_filtered)

### NOW can finally try a correlation matrix ? 
## corr <- round(cor(agegap2_filtered))
#View(corr)
#ggcorrplot(corr)
######## okay this doesnt really make sense/work 

#### try scatterplot but adjusting the size of the points 
## found bubble plot on ggplot extensions 
AgeGapPlot <- ggplot(agegaps, aes(x=actor_1_age, 
                                  y=actor_2_age, 
                                  size = age_difference, 
                                  color = age_difference)) +
  scale_color_gradient(high ="purple", low ="pink") +
  geom_point(alpha=0.7) + 
  facet_wrap(character_1_gender~character_2_gender) + 
  theme_bw() + 
  theme(axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), 
        plot.title=element_text(hjust=0.5)) +
  labs(x="Age of Actor 1", 
       y="Age of Actor 2", 
       title="Age Gaps Between Hollywood Actxrs Faceted by Character Gender") ##x in Actxrs is intentional inclusive language :)

##now try to make it interactive! 
## but come back to it because its been more than one hour and we are taking healthy coding breaks 
# good job 
praise()

##making it interactive 
IntAgeGapPlot <- ggplotly(AgeGapPlot, tooltip="text")
IntAgeGapPlot
ggsave(here("HollywoodAgeGaps","Outputs", "agegaps_plot.png"))
## not exactly sure how to get the hover-over-data-points to work but it's a start! 

