## STAT 7130 Class 5 R Code ##

library(tidyverse)

## Changing Color Palettes Manually ##

## Suppose I want to Build a Horizontal Bar
## Chart for total number of HRs hit by 
## NL East Team during the 2022 Season ##

nl_east <- Lahman::Batting |>
  filter(yearID == 2022 &
           teamID %in% c("ATL","NYN","WAS",
                         "MIA","PHI")) |>
  group_by(teamID) |>
  summarize(HR = sum(HR,na.rm=T))

nl_east |>
  ggplot(aes(x=HR,y=reorder(teamID,HR),fill=teamID)) +
  geom_bar(stat='identity',color='black') +
  labs(x="Total Team Homeruns Hit",
       y="National League East Team",
       title="Total Homeruns Hit for NL East Teams",
       subtitle="2022 Regular Season") +
  theme_minimal()

## It'd be nice if the colors matched the team colors ##

braves <- "#CE1141"
mets <- "#FF5910"
phillies <- "#E81828"
nats <- "#14225A"
marlins <- "#00A3E0"

Colors <- tibble(Teams = c("ATL","NYN","PHI",
                           "WAS","MIA"),
                 Colors = c(braves,mets,phillies,
                            nats,marlins))

nl_east1 <- nl_east |>
  left_join(Colors,by=c("teamID" = "Teams"))

nl_east1 |>
  ggplot(aes(x=HR,y=reorder(teamID,HR),fill=teamID)) +
  geom_bar(stat='identity',color='black') +
  geom_text(aes(label=HR),hjust=1.25,color='white') +
  labs(x="Total Team Homeruns Hit",
       y="National League East Team",
       title="Total Homeruns Hit for NL East Teams",
       subtitle="2022 Regular Season") +
  theme_minimal() +
  theme(legend.position = 'none') +
  scale_fill_manual(values=nl_east1$Colors)

## Adding Labels ##

## A Descriptive Title ##

mpg |>
  ggplot(aes(x=displ,y=hwy)) + 
  geom_point(aes(color=class)) +
  geom_smooth(se = FALSE) +
  labs(title = paste("Fuel efficiency generally decreases",
                  "with engine size",sep=" "))

## Adding a Subtitle & Caption ##

p <- mpg |>
  ggplot(aes(x=displ,y=hwy)) + 
  geom_point(aes(color=class)) +
  geom_smooth(se = FALSE) +
  labs(
    title = paste("Fuel efficiency generally decreases",
                  "with engine size",sep=" "),
    subtitle = paste("Two seaters (sports cars) are an",
                     "exception because of their light weight",sep=" "),
    caption = "Data from fueleconomy.gov")

p

p + labs(x = "Engine Displacement (L)",
         y = "Highway Miles per Gallon",
         color = "Car Type") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## Adding Mathematical Symbols/Equations to Labels ##

sigma <- 5

n <- seq(2,100,by=1)

ev <- 5*sqrt(2/(n-1))*gamma(n/2)/gamma((n-1)/2)

ggplot() + 
  geom_line(aes(x=n,y=ev),color="black") +
  geom_line(aes(x=n,y=sigma),color="red") +
  labs(x = "Sample Size",
       y = "Expected Value",
       title = expression(
         paste("Sample Standard Deviation",
               "Is an Asymptotically Unbiased ",
               "Estimator of ",sigma)))

## Another Math Symbol Example ##

df <- tibble(x = runif(10),
             y = runif(10))

df |> ggplot(aes(x,y)) +
  geom_point() +
  labs( 
    x = quote(sum(x[i]^2,i==1,n)),
    y = quote(alpha + beta + frac(delta,theta)))

## Annotations ##

label_info <- mpg |>
  group_by(drv) |>
  arrange(desc(displ)) |>
  slice_head(n = 1) |>
  mutate(
    drive_type = case_when(
      drv == "f" ~ "front-wheel drive",
      drv == "r" ~ "rear-wheel drive",
      drv == "4" ~ "4-wheel drive"
    )
  ) |>
  select(displ, hwy, drv, drive_type)

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_text(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, hjust = "right", vjust = "bottom"
  ) +
  theme(legend.position = "none")

## Fix Overlapping Data Labels ##

library(ggrepel)

mpg |>
  ggplot(aes(x = displ, y = hwy, color = drv)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  geom_label_repel(
    data = label_info, 
    aes(x = displ, y = hwy, label = drive_type),
    fontface = "bold", size = 5, nudge_y = 2
  ) +
  theme(legend.position = "none")

## Adding Annotations to Bar Chart ##

## Remember this chart? ##

diamonds |>
  ggplot(aes(x=cut)) + geom_bar()

## Let's add the raw frequencies and relative frequencies to ##
## be above each bar ##

cuts_df <- diamonds |>
  group_by(cut) |>
  count() |>
  mutate(label =  paste(n,", ",
                        round(n/nrow(diamonds)*100,2),"%",sep=""))

cuts_df |>
  ggplot(aes(cut,n)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = label),vjust=-0.25)

## Using Annotations Instead of a Legend ##

## Before ##

palmerpenguins::penguins |>
  ggplot(aes(bill_length_mm,bill_depth_mm)) +
  geom_point(aes(color=species))

## After ##

species_avg <- palmerpenguins::penguins |>
  group_by(species) |>
  summarize(bill_length_mm = median(bill_length_mm,na.rm=T),
                   bill_depth_mm = median(bill_depth_mm,na.rm=T))

palmerpenguins::penguins |>
  ggplot(aes(bill_length_mm,bill_depth_mm,color=species)) +
  geom_point() +
  geom_label_repel(aes(label=species),data=species_avg,
                            size = 6, label.size = 0,
                            segment.color = NA
                            ) +
  theme(legend.position = "none")

## MPG Example ##
  
class_avg <- mpg |>
  group_by(class) |>
  summarize(displ = median(displ),
                   hwy = median(hwy))

mpg |>
  ggplot(aes(displ,hwy,color=class)) +
  geom_point() +
  geom_label_repel(aes(label=class),data=class_avg,
                            size = 6, label.size = 0,
                            segment.color = NA
  ) +
  theme(legend.position = "none")

## Adding a Single Data Label ##

label <- mpg |>
  summarize(
    displ = max(displ),
    hwy = max(hwy),
    label = paste(
      "Increasing engine size is \n related to",
      "decreasing fuel economy"
    )
  )

ggplot(mpg,aes(displ,hwy)) +
  geom_point() +
  geom_text(
    aes(label = label),
    data = label,
    vjust = "top",
    hjust = "right"
  )

## Adjusting Scales ##

## Changing the Increments of Tick Marks on Y axis ##

mpg |>
  ggplot(aes(displ,hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15,40,by=5))

palmerpenguins::penguins |>
  ggplot(aes(bill_length_mm,bill_depth_mm)) +
  geom_point() +
  scale_y_continuous(breaks = seq(13.50,22.50,by=0.50)) +
  scale_x_continuous(breaks = seq(30,60,by=5)) +
  theme(axis.title.y = element_text(angle = 45,vjust=0.75))

## Suppressing Tick Mark Labels ##

mpg |>
  ggplot(aes(displ,hwy)) +
  geom_point() + 
  scale_x_continuous(labels = NULL) +
  scale_y_continuous(labels = NULL)

## Creating Tick Mark Labels with Unequal Widths ##

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(start,id)) +
  geom_point() +
  geom_segment(aes(xend = end, yend = id)) +
  labs(x = "Inauguration Year",
       y = "Number of President") +
  scale_x_date(
    breaks = presidential$start,
    date_labels = "'%y"
  ) +
  scale_y_continuous(breaks = seq(34,45,by=1))

## Moving the Legend ##

base <- mpg |>
  ggplot(aes(displ,hwy)) +
  geom_point(aes(color = class))

base + theme(legend.position = "left")
base + theme(legend.position = "top")
base + theme(legend.position = "bottom")
base + theme(legend.position = "right")
base + theme(legend.position = "none")

## Modifying Legend Appearance ##

base + geom_smooth(se = FALSE) +
  theme(legend.position = "bottom") +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 10)
    )
  )

## Changing Presidential Plot Colors by Party ##

presidential |>
  mutate(id = 33 + row_number()) |>
  ggplot(aes(start,id,color=party)) +
  geom_point() + 
  geom_segment(aes(xend = end, yend = id)) +
  scale_color_manual(values = c(Republican = "red",
                                Democratic = "blue"))

## Example with Histograms ##

penguins <- palmerpenguins::penguins

## Quantitative Analysis ##

## Summary Statistics ##

penguins |>
  group_by(species) |>
  summarize(Mean = mean(body_mass_g,na.rm=T),
                   `Standard Deviation` = sd(body_mass_g,na.rm=T),
                   Q1 = quantile(body_mass_g,probs=0.25,na.rm=T),
                   Q2 = median(body_mass_g,na.rm=T),
                   Q3 = quantile(body_mass_g,probs=0.75,na.rm=T))

penguins |>
  group_by(species) |>
  rstatix::get_summary_stats(body_mass_g,type='common')

## Histogram ##

## Base R Method ##

hist(penguins$body_mass_g,
     xlab = "Body Mass (in grams)",
     main = "Histogram of Penguin Body Mass")

## ggplot2 Method ##

ggplot(penguins,aes(x=body_mass_g)) + geom_histogram()

## Change Number of Bins ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(bins=nclass.Sturges(penguins$body_mass_g))

## Change Binwidth ##

ggplot(penguins,aes(x=body_mass_g)) + geom_histogram(binwidth = 575)

## Make it More Aesthetically Pleasing ##

## Add Better Titles ##

p <- ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(bins=nclass.Sturges(penguins$body_mass_g))

p <- p + labs(x = "Body Mass (in Grams)",
         y = "Frequency",
         title = "Histogram of Penguin Body Mass")
p

## Give Bars a White Outline ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(bins=10,color="white") +
  labs(x = "Body Mass (in Grams)",
       y = "Frequency",
       title = "Histogram of Penguin Body Mass")

## Change Fill of Bars to White ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(bins=10,color="steelblue",fill="white") +
  labs(x = "Body Mass (in Grams)",
       y = "Frequency",
       title = "Histogram of Penguin Body Mass")

## Center Histogram Title ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(bins=10,color="steelblue",fill="white") +
  labs(x = "Body Mass (in Grams)",
       y = "Frequency",
       title = "Histogram of Penguin Body Mass") +
  theme(plot.title=element_text(hjust=0.50))

## Remove Grid Using theme_classic() ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(bins=10,color="steelblue",fill="white") +
  labs(x = "Body Mass (in Grams)",
       y = "Frequency",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Add Density Plot ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="steelblue",fill="white") +
  geom_density() +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Color Density Line Blue ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="steelblue",fill="white") +
  geom_density(color="steelblue") +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Fill in Entire Plot Blue ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue") +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Whoops! We need to change the opacity using alpha ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue",alpha=0.4) +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Add Mean ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue",alpha=0.4) +
  geom_vline(aes(xintercept=mean(body_mass_g, na.rm=T))) + 
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Kinda hard to see...let's change the color and linetype ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue",alpha=0.4) +
  geom_vline(aes(xintercept=mean(body_mass_g, na.rm=T)),
             color="red",linetype="dashed") + 
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## How can someone know what that line represents? ##

## Let's add a label! ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue",alpha=0.4) +
  geom_vline(aes(xintercept=mean(body_mass_g, na.rm=T)),
             color="red",linetype="dashed") + 
  geom_text(aes(x = mean(body_mass_g,na.rm=T),
                label = "Mean Body Mass",
                y = 0.0006)) +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## It needs to be moved, maybe to the right? ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue",alpha=0.4) +
  geom_vline(aes(xintercept=mean(body_mass_g, na.rm=T)),
             color="red",linetype="dashed") + 
  geom_text(aes(x = mean(body_mass_g,na.rm=T),
                label = "Mean Body Mass",
                y = 0.0006),hjust=-0.10) +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## The color should be the same as the line to make it more clear ##

ggplot(penguins,aes(x=body_mass_g)) + 
  geom_histogram(aes(y=..density..),bins=10,color="black",fill="white") +
  geom_density(color="blue",fill="blue",alpha=0.4) +
  geom_vline(aes(xintercept=mean(body_mass_g, na.rm=T)),
             color="red",linetype="dashed") + 
  geom_text(aes(x = mean(body_mass_g,na.rm=T),
                label = "Mean Body Mass",
                y = 0.0006),
            hjust=-0.10,
            color="red") +
  labs(x = "Body Mass (in Grams)",
       y = "Density",
       title = "Histogram of Penguin Body Mass") +
  theme_classic() + 
  theme(plot.title=element_text(hjust=0.50))

## Adding Annotations to a 100% Stacked Bar Chart ##

penguins |>
  na.omit() |>
  group_by(species,sex) |>
  count() |>
  rename(`Sex Frequency` = n) |>
  left_join(
    penguins |>
      na.omit() |>
      group_by(species) |>
      count() |>
      rename(`Species Frequency` = n)
  ) |>
  mutate(Proportion = `Sex Frequency`/`Species Frequency`) |>
  ggplot(aes(x=species,y=Proportion,fill=sex)) +
  geom_bar(stat='identity')
