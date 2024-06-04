## STAT 7130 Class 4 R Code ##

library(tidyverse)

## Plotting Basic ggplot ##

install.packages('palmerpenguins')

penguins <- palmerpenguins::penguins

## Summary of Penguins dataset ##

penguins |>
  glimpse()

## Building the ggplot canvas ##

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm,
                     y = body_mass_g))

## Add a geom ##

ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm,
                     y = body_mass_g)) +
  geom_point()

## MPG Example ##

mpg <- ggplot2::mpg

p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy))

p + labs(x = "Engine Displacement",
         y = "Highway MPG")

## Scatterplot by Group (Using the Color aesthetic) ##

## Examine MPG dataset ##

mpg |>
  glimpse()

## Differentiate Class by Color of Points! ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,color=class))

ggplot(data = mpg,mapping = aes(x = displ,y = hwy,color=class)) + 
  geom_point()

mpg |>
  ggplot(mapping = aes(x = displ,y = hwy,color=class)) + 
  geom_point()

## Differentiate Species also by Color of Points ##

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           color=species))

## Scatterplot by Group (Using the Size aesthetic) ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,size=class))

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           size=species))

## Scatterplot by Group (Using the alpha aesthetic) ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,alpha=class))

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           alpha=species))

## Scatterplot by Group (Using the Shape aesthetic) ##

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ,y = hwy,shape=class))

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                           shape=species))

## Change the color of the points ##

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy),
             color='blue')

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm),
             color='#CE1141')

## Let's check out all the colors available in R ##

colors()

"http://sape.inf.usi.ch/quick-reference/ggplot2/colour"

## Have point shape be uniform ##

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm),
             shape = 11) +
  xlim(0,100)

## Facets ##

## Wraps ##

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class,nrow=3)

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  facet_wrap(~species,nrow=3)

## Illustrate Why Wraps are handy ##

## Subset Adelie & Chinstrap Penguins ##

adelie <- penguins |>
  filter(species == "Adelie")

chinstrap <- penguins |>
  filter(species == "Chinstrap")

gentoo <- penguins |>
  filter(species == "Gentoo")

ag <- ggplot(data = adelie) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

cg <- ggplot(data = chinstrap) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

gg <- ggplot(data = gentoo) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

install.packages('gridExtra')

gridExtra::grid.arrange(ag,cg,gg,ncol=1)

## Grids ##

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ cyl)

ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  facet_grid(island~year)

p <- ggplot(data = penguins) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

p
  
p + facet_grid(island~year)

## Using Different Geoms ##

## Smoothing ##

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = penguins) +
  geom_smooth(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

## Not every aes() argument works with every geom ##

## This won't work ##

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy,shape=cyl))

## This will ##

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype=drv))

ggplot(data = penguins) +
  geom_smooth(mapping = aes(x = bill_length_mm, y = bill_depth_mm,
                            linetype = species))

## Using Multiple Geoms Together ##

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = penguins) +
  geom_smooth(mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(mapping = aes(x = bill_length_mm, y = bill_depth_mm))

## Specify Global Aesthetics ##

ggplot(data=mpg,aes(x=displ,y=hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data=penguins,aes(x=bill_length_mm,y=bill_depth_mm)) +
  geom_point() +
  geom_smooth()

## Using Local Aesthetics Alongside Global Aesthetics ##

ggplot(data=mpg,aes(x=displ,y=hwy)) + 
  geom_point(aes(color=class)) + 
  geom_smooth()

ggplot(data=penguins,aes(x=bill_length_mm,y=bill_depth_mm)) +
  geom_point(aes(color=species)) +
  geom_smooth(aes(linetype=species))

## Using dplyr inside local aesthetics ##

ggplot(data=penguins,aes(x=bill_length_mm,y=bill_depth_mm)) +
  geom_smooth(data = penguins |>
                filter(island == "Biscoe")
              ) +
  geom_point(aes(color = island))

## Using dplyr with global aesthetics ##

penguins |>
  filter(island == "Biscoe" & species == "Gentoo") |>
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
    geom_smooth() + 
    geom_point()

mpg |>
  filter(class == 'subcompact') |>
  ggplot(aes(x=displ,y=hwy)) +
    geom_smooth() +
    geom_point(data = mpg,aes(x = displ,y=hwy,color = class))
  
## Using dplyr with global aesthetics inside ggplot ##

ggplot(data = 
         penguins |>
         filter(island == "Biscoe" &
                species == "Gentoo"),
       aes(x = bill_length_mm,y=bill_depth_mm)) +
  geom_smooth() +
  geom_point()

## Using Statistical Transformations ##

## Plot a Bar Chart ##

diamonds <- ggplot2::diamonds

## Summarize diamonds ##

diamonds |>
  glimpse()

ggplot(diamonds,aes(x=cut)) +
  geom_bar()

## Using the stat_count() Function ##

ggplot(diamonds,aes(x=cut)) +
  stat_count()

## Changing the Default Stat ##

## Count Up Number of Cuts in Diamonds Dataframe ##

cuts_df <- diamonds |>
  group_by(cut) |>
  count()

cuts_df

ggplot(cuts_df) +
  geom_bar(aes(x=cut,y=n),stat="identity")

## Position Adjustments ##

## Default for geom_bar is position="stack" ##

ggplot(diamonds) + 
  geom_bar(aes(x=cut,fill=clarity))

## Change to position = "fill" ##

ggplot(diamonds) + 
  geom_bar(aes(x=cut,fill=clarity),position="fill")

## Change to position = "dodge" ##

ggplot(diamonds) + 
  geom_bar(aes(x=cut,fill=clarity),position="dodge")

## Using the "jitter" position argument for geom_point ##

ggplot(mpg) + 
  geom_point(aes(displ,hwy))

ggplot(mpg,aes(x=displ,y=hwy)) +
  geom_point(position="jitter")

ggplot(mpg,aes(x=displ,y=hwy)) +
  geom_point()

## Changing Axes Titles and Adding a Main Title/Subtitle ##

p <- ggplot(mpg,aes(x=displ,y=hwy)) +
  geom_point(position="jitter") + 
  labs(x = "Engine Displacement",
       y = "Highway Miles Per Gallon (MPG)",
       title = "Engine Displacement vs Highway MPG",
       subtitle = "An Example of a Scatterplot")

p

## Center Justifying a Title/Subtitle ##

p + theme(plot.title=element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))

## Changing Themes ##

p + theme_bw()
p + theme_classic()
p + theme_minimal()

library(ggthemes)

p + theme_wsj()
p + theme_excel_new()
  
## Practice Problem ##

## Austin Riley's 2022 Hit Breakdown ##

## First, we have to figure out his playerID ##

ar_id <- Lahman::People |>
  filter(nameFirst == "Austin" & nameLast == "Riley") |>
  select(playerID,nameLast,nameFirst)

## Now, we want to subset his 2022 stats from the full batting dataframe ##

batting <- Lahman::Batting |> 
  filter(yearID == "2022" & playerID == ar_id$playerID)

## Check that it worked ##

batting

## Create an aggregated tibble ##

new_df <- tribble(
  ~`Hit Type`, ~Frequency,
  "Singles",batting$H - sum(batting$X2B,batting$X3B,batting$HR),
  "Doubles",batting$X2B,
  "Triples",batting$X3B,
  "Homeruns",batting$HR
)

## Check that this worked ##

new_df

new_df |>
  mutate(Proportion = Frequency/sum(Frequency)) |>
  ggplot(aes(y=reorder(`Hit Type`,Proportion),x=Proportion)) +
  geom_bar(stat="identity") + 
  labs(title = "Austin Riley's 2022 Hits",
                    subtitle = "By Hit Type",
       y="Hit Type") +
  theme_classic()
