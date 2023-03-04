#############################################
### Tidy Tuesdays, Week 2 
### Created by: Hannah Merges
### Created on: 2023-02-21
### Last updated: 2023-02-26

##########################
#### Load libraries 
###########################
library(tidyverse)
library(here)
library(praise)
library(beyonce)
library(ggthemes)
library(devtools)
library(memer)
library(cropcircles)
library(patchwork) ##package that allows you to add plots together 
library(treemap)
library(beyonce)
library(BobRossColors)

#################################
#### Read in Data ####
#################################
Bob_Ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
View(Bob_Ross)

################################################
### making a meme ###
##############################################
##okay how do we want to visualize this data 
## but first... insert a meme! learning something new 
devtools::install_github("sctyner/memer")
meme_list()
meme_get("NoneOfMyBusiness") %>% 
  meme_text_bottom("When you learn how to create a meme in R") 
#install.packages("memery") ##this could help me bring an any image and therefore be able to use Bob Ross 

##some inspiration from Twitter's Tidy Tuesday 
# devtools::install_github("doehm/cropcircles")
# this could be cool to circle back to at some point 
## hex_imgs <- hex_crop(Bob_Ross, border_size = 16, border_colour = "black")
## would like to learn how to do this

#########################################
#### wrangling and tidying the data ####
########################################
##this shows you an example of the Bob Ross color palette 
devtools::install_github("frankiethull/BobRossColors")
show_colors_and_paintings()
??BobRossColors
unique_bob_ross_colors

## can install patchwork package to make plots in ggplot and combine them 
devtools::install_github("thomasp85/patchwork")
## use this because want to combine one chart that 

Bob_Ross_long <- Bob_Ross %>% 
  pivot_longer(cols=Black_Gesso:Alizarin_Crimson, 
               names_to="bobrosscolors", 
               values_to="logical") %>% 
  group_by(bobrosscolors) %>% 
  summarise(count=sum(logical)) #this adds all the counts together into one column for total # of time each color is used
View(Bob_Ross_long)

##############################
## plotting with tree maps 
###############################
##try a tree map to represent colors in different paintings 
##treemap(Bob_Ross,
# index="colors",
# vSize="num_colors",
# type="index") 
##okay that didn't work because maybe need to separate the colors column by comma 
#and maybe change color by the Bob_Ross color palette? 

##trying different visualizations with treemap 
#treemap(Bob_Ross,
        #index="season",
        #vSize="num_colors",
        #type="index")

treemap(Bob_Ross_long,
        index="bobrosscolors", #these are each of the colors used in the painting 
        vSize="count", #this has to be a numeric value 
        type="index", 
        title="Frequency of Colors Used in Bob Ross Paintings", 
        palette="Set3", #there is a way to create your own but I have not gotten there yet 
        fontsize.labels=(12),
        fontcolor.labels("black")) #changes the color of the labels once I figure that out...
  
help(RColorBrewer)
display.brewer.pal()
##not loading text -- error - something with the font?? 
help(treemap)
loadfonts(dev="win")

#########################################
## plotting with bar plots ##
#########################################
##try adding treemap to a basic ggplot to practice using patchwork 

ggplot(Bob_Ross_long, 
       aes(x=bobrosscolors,
           y=count,
           fill=bobrosscolors)) + 
  geom_bar(stat = "identity", width=0.7) + #width controls width of individual bars 
  coord_flip() + #flips axes 
  scale_fill_manual(values = c("red", "green", "blue")) ##here try to figure out how to get Bob Ross palette working 

#want to highlight the biggest one 
## Add a column indicating whether the category should be highlighted
Bob_Ross_longer <- Bob_Ross_long %>% 
  mutate(ToHighlight = ifelse(count == 400, "yes", "no"))
View(Bob_Ross_longer)
  
ggplot(Bob_Ross_longer, 
       aes(x=bobrosscolors,
           y=count,
           fill=ToHighlight)) + 
  geom_bar(stat = "identity", width=0.7) + #width controls width of individual bars 
  coord_flip() + #flips axes
  scale_fill_manual(values = c("yes"="tomato", "no"="grey" ), guide = FALSE) + #manual fill colors based on new highlight column
  theme_linedraw() + #classic themes and axes labeling 
  labs(y="Total Number of Times Each Color was Used", 
       x="Colors", 
       title="Colors Used in Each Painting (counted from episodes and seasons)") + 
  theme(axis.text.x=element_text(size=10), 
        axis.text.y=element_text(size=10), 
        axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), 
        plot.title=element_text(hjust=0.5))
  ggsave(here("BobRoss_Week2","Outputs","ColorBarPlot.png"))

#it worked! now figure out how to change gray to a Bob Ross color ?? 
  ##keep this plot and save for TidyTues assignment 

  
#############################################
## overall conclusions and summary ##
###########################################
#running close to time limit and there is so much more I want to figure out: 
# how to fix error in treemap 
# combine treemap with bar plot *** 
# get bob ross color palette working in scale_fill 
# but still learned a lot this week 




  




