## STAT 7130 More Advanced ggplot2 Functionality ##

library(tidyverse)

## Arranging Multiple Plots using patchwork ##

## Suppose I want to visualize 3 different plots 
## from the penguins data: ##

## (1): A Scatterplot comparing bill length
## and flipper length differentiating sex by 
## point color ##

penguins <- palmerpenguins::penguins |>
  na.omit()

point_plot <- penguins |>
  ggplot(aes(bill_length_mm, flipper_length_mm, color = sex)) +
  geom_point() +
  labs(x = "Bill Length (in mm)",
       y = "Flipper Length (in mm)",
       color = "Sex") +
  theme_light()

point_plot

## (2): A Scatterplot comparing bill length
## and bill depth differentiating sex by
## point color ##

point_plot2 <- penguins |>
  ggplot(aes(bill_length_mm, bill_depth_mm, color = sex)) +
  geom_point() +
  labs(x = "Bill Length (in mm)",
       y = "Bill Depth (in mm)",
       color = "Sex") +
  theme_light()

point_plot2

## (3): A horizontal boxplot comparing body mass
## by sex ##

boxplot_plot <- penguins |> 
  ggplot(aes(x = body_mass_g, fill = sex)) +
  geom_boxplot() +
  labs(x = "Body Mass (in grams)",
       fill = "Sex") + 
  theme_light() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

boxplot_plot

## Clearly, all of these plots are interrelated. Suppose
## I want them all to be a single image (i.e., one jpeg file) 
## rather than 3 separate figures. How do we do this?? ##

## Super old school technique: ##

ex1 <- rnorm(100,mean=1)
ex2 <- rnorm(100,mean=2)
ex3 <- rnorm(100,mean=100)

par(mfrow=c(2,2))
hist(ex1)
hist(ex2)
hist(ex3)
dev.off()

## Classical Way: grid.arrange ##

install.packages('gridExtra')

gridExtra::grid.arrange(point_plot,point_plot2,boxplot_plot,
                        nrow=2)

## Looks okay, but it isn't ideal because the bottom boxplot
## doesn't span the two columns ##

## This is where the beauty and ease of the patchwork package
## comes into play. Let's install and then check it out! ##

install.packages('patchwork')

library(patchwork)

p1 <- (point_plot + point_plot2)/boxplot_plot

p1

## See? Much easier! But there is still a bit to be desired.
## Notice how the scatterplots have a redundant legend. How 
## can we keep just the one on the right? ##

p1 + plot_layout(guides = "collect") 

## Well now, we really don't need two legends at all since
## the colors differentiate the sexes. We can remove
## the boxplot legend and then move the scatterplot
## legend to above the entire figure ##

boxplot_plot <- boxplot_plot + guides(fill='none')

boxplot_plot

p1 <- (point_plot + point_plot2)/boxplot_plot

p1 + plot_layout(guides = 'collect') & theme(legend.position = 'top')

## Make it more APA style?? ##

p1 + plot_layout(guides='collect') +
plot_annotation(
  title = "Look at that arrangement!",
  subtitle = "Wow",
  caption = "So awesome!",
  tag_levels = "A",
  tag_prefix = "(",
  tag_suffix = ")"
) & 
  theme(legend.position = "top")

## Let's do another, simpler example. Let's create 3 different
## charts and combine them using patchwork. Using the 2022 
## NL East pitching data: ##

pitch <- Lahman::Pitching |>
  filter(yearID == 2022 & teamID %in% c('ATL','NYN','PHI',
                                          'WAS','MIA')) |>
  mutate(yearID = as.character(yearID)) |>
  filter(ERA < 10 & BAOpp > 0)

## (1): Vertical Boxplot of ERA with different colors 
## for the different teams ##

pitch1 <- pitch |>
  ggplot(aes(y=ERA,color=teamID)) +
  geom_boxplot() +
  labs(y = "Earned Run Average",
       color = "Team") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

pitch1

## (2): Vertical Boxplot of BAOpp with different colors
## for the different teams ##

pitch2 <- pitch |>
  ggplot(aes(y=BAOpp,color=teamID)) +
  geom_boxplot() +
  labs(y = "Opponent Batting Average",
       color = "Team") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

pitch2

## (3): Ordered Horizontal Bar chart for Saves ##

save_df <- pitch |>
  group_by(teamID) |>
  summarize(Saves = sum(SV))

pitch3 <- save_df |>
  ggplot(aes(x=Saves,y=reorder(teamID,Saves),fill=teamID)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=Saves),hjust=1.25,color='white') +
  labs(x= "Saves",
       y = "Team") +
  theme_minimal() +
  theme(legend.position='none') 

pitch3 

## Now, let's 'patch' it together ##

pitch3/(pitch1 + pitch2) + plot_layout(guides='collect') +
  plot_annotation(
    title = "2022 National League East Pitching Statistics"
  )

## Checkout https://patchwork.data-imaginist.com/ to learn more 
## about patchwork ##

## Communicating Changes Over Time with gganimate ##

## Situation: Suppose I want to see how the average number 
## of strikeouts per nine innings pitched (SO/9) MLB pitchers 
## have thrown has changed, if at all, from the 1910 season to the 2022 season
## by lgID ##

so9 <- Lahman::Pitching |>
  filter(between(yearID,1910,2022) & (IPouts/3) >= 15) |>
  mutate(SO9 = SO/(IPouts/3)*9) |>
  group_by(yearID,lgID) |>
  summarize(Mean_SO9 = round(mean(SO9,na.rm=T),2)) |>
  filter(lgID %in% c('AL','NL'))

## Install and Load the gganimate & gifski packages ##

install.packages('gganimate')
install.packages('gifski')

library(gganimate)

so9 |>
  ggplot(aes(x=Mean_SO9,y=lgID)) +
  geom_bar(stat='identity',color='red',fill='white') +
  geom_text(aes(label=Mean_SO9),color='red',fontface='bold',
            hjust=-1) +
  labs(title = 'Strikeouts Per 9 Innings Pitched by League',
       subtitle = 'Season: {frame_time}',
       x = "Mean Strikeouts Per 9 Innings Pitched",
       y = "League") +
  theme_classic() +
  scale_x_continuous(limits=c(0,10)) +
  transition_time(yearID)

## Save the gif ##

anim_save("Strikeouts Per 9 Over Time.gif")

## Let's go through another example: using the ggplot2::txhousing
## dataframe, suppose we want to visualize the changes in average median home
## price for the cities of Austin, Houston, Dallas, and San Antonio
## for each month in 2010 ##

## Get the data into the right format ##

txdata <- ggplot2::txhousing |>
  filter(year == 2010 & 
                  city %in% c("Austin","Houston",
                              "Dallas","San Antonio")) |>
  group_by(month,city) |>
  summarize(Mean_Median = round(mean(median,na.rm=T)))

options(scipen=999) # this will prevent the chart from rendering in scientific notation #

## Generate a dynamic horizontal dot chart ##

txdata |>
  ggplot(aes(x=Mean_Median,y=city)) +
  geom_point(color='#BF5700',size=5) +
  geom_segment(aes(yend=city),xend=0,linetype='dashed',
               color='#333F48') +
  labs(title = 'Average Median Sold Home Price for Texas Cities in 2010',
       subtitle = 'Month: {frame_time}',
       x = "Average Median Price",
       y = "City") +
  theme_classic() +
  scale_x_continuous(limits=c(0,250000)) +
  transition_time(month)

## Save the gif ##

anim_save("Texas Home Price Dots.gif")

## Creating dynamic graphics using plotly ##

## Let's use the MPG data again ##

mpg <- ggplot2::mpg

mpg |>
  ggplot(aes(displ,hwy)) +
  geom_point(aes(color = class),position='jitter') +
  geom_smooth(se = FALSE) +
  labs(x = "Engine Displacement",
       y = "Highway Miles Per Gallon",
       color = "Car Type") +
  theme_bw()

## It would be nice if we could hover our cursor over a point and
## see what the exact details for the given point were. Enter plotly :) ##

install.packages('plotly')
library(plotly)

mpg_plot <- mpg |>
  ggplot(aes(displ,hwy)) +
  geom_point(aes(color = class),position='jitter') +
  geom_smooth(se = FALSE) +
  labs(x = "Engine Displacement",
       y = "Highway Miles Per Gallon",
       color = "Car Type") +
  theme_bw()

ggplotly(mpg_plot)

## Challenge: Create a plotly scatterplot using Lahman::Batting
## where we examine HR/SO for players with more than 150 AB
## during the 2022 regular season. Let's consider only the top
## 30 HR hitters ##

plotly_df <- Lahman::Batting |>
  left_join(Lahman::People |>
              select(playerID,nameFirst,nameLast) |>
              mutate(Name = paste(nameFirst,nameLast)) |>
              select(playerID,Name)) |>
  filter(yearID == 2022 & AB >= 150) |>
  arrange(desc(HR)) |>
  slice_head(n=30)

library(ggrepel)

hr_plot <- plotly_df |>
  ggplot(aes(x=HR,y=SO)) +
  geom_point(aes(text=Name)) +
  labs(x = "Homeruns Hit",
       y = "Strikeouts") +
  theme_light()

hr_plot

ggplotly(hr_plot)
