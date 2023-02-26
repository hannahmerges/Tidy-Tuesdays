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
library(memery)
library(cropcircles)
library(patchwork)
library(treemap)
#library(showtext)
#library(janitor)
#library(glue)
#library(ggtext)
#library(ggpath)
library(BobRossColors)

#################################
#### Read in Data ####
#################################
Bob_Ross <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
View(Bob_Ross)

##okay how do we want to visualize this data 
## but first... insert a meme! learning something new 
devtools::install_github("sctyner/memer")
meme_list()
meme_get("SuccessKid") %>% 
  meme_text_bottom("When you learn how to create a meme in R") 
#install.packages("memery") ##this could help me bring an any image and therefore be able to use Bob Ross 

##some inspiration from Twitter's Tidy Tuesday 
# devtools::install_github("doehm/cropcircles")
# this could be cool to circle back to at some point 
## hex_imgs <- hex_crop(Bob_Ross, border_size = 16, border_colour = "black")
## would like to learn how to do this

#########################################
#### plotting the data ####
########################################

##this shows you an example of the Bob Ross color palette 
devtools::install_github("frankiethull/BobRossColors")
show_colors_and_paintings()

## can install patchwork package to make plots in ggplot and combine them 
devtools::install_github("thomasp85/patchwork")
## use this because want to combine one chart that 

##try a tree map to represent colors in different paintings 
treemap(Bob_Ross,
        index="colors",
        vSize="num_colors",
        type="index") 

##okay that didn't work because maybe need to separate the colors column by comma 
#and maybe change color by the Bob_Ross color palette? 

Bob_Ross_long <- Bob_Ross %>% 
  pivot_longer(cols=Black_Gesso:Alizarin_Crimson, 
               names_to="bobrosscolors", 
               values_to="logical") %>% 
  group_by(bobrosscolors) %>% 
  summarise(count=sum(logical)) #this adds all the counts together into one column for total # of time each color is used
View(Bob_Ross_long)

##trying different visualizations with treemap 
#treemap(Bob_Ross,
        #index="season",
        #vSize="num_colors",
        #type="index")

treemap(Bob_Ross_long,
        index="bobrosscolors", #these are each of the colors used in the painting 
        vSize="count", #this has to be a numeric value 
        type="index", 
        fontsize.labels=(12), 
        fontcolor.labels("black"))


        
##not loading text -- error with that plus still want to change colors -- 
##figure that out and try adding to a basic ggplot to practice using patchwork 

##other code for treemaps 
fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
fontcolor.labels=c("white","orange"),    # Color of labels
fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
bg.labels=c("transparent"),              # Background color of labels
align.labels=list(
  c("center", "center"), 
  c("right", "bottom")
),                                   # Where to place labels in the rectangle?
overlap.labels=0.5,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
inflate.labels=F,                        # If true, labels are bigger when rectangle is bigger.

)         

loadfonts(dev="win")


##make a bar plot 

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
  scale_fill_manual(values = c("yes"="tomato", "no"="gray" ), guide = FALSE)
  ggsave(here("BobRoss_Week2","Outputs","highlightedbarplot.png"))


#it worked! now figure out how to change gray to a Bob Ross color 

##running close to time limit and there is so much more I want to figure out: 
# how to fix error in treemap 
# combine treemap with bar plot *** 
# get bob ross color palette working in scale_fill 
# but still learned a lot this week 




  




