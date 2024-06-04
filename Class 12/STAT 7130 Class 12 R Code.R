## STAT 7130 Class 12 Code - String Variables ##

library(tidyverse)

## String Variables are simply strings of text! ##

funny_shows <- c("Arrested Development",
                 "The Office",
                 "Curb Your Enthusiasm")

funny_shows

## Suppose we wanted to count the number of words in each element of 
## this character vector ##

# install.packages('stringr')
library(stringr)

sapply(str_split(funny_shows," "),length)

## What if we wanted to know the number of characters in each string? ##

## AD should have 20 ##

str_length(funny_shows)

## We can also use base R for this: ##

nchar(funny_shows)

## Let's look at an applied example using the baseball.xlsx dataset ##

baseball <- readxl::read_xlsx('baseball.xlsx')

## Examine the Structure of name ##

baseball$Name[1:5]

## So we have lastname and then firstname separated by a comma and a space ##

## Let's use str_split once again but change our delimiter to ', ' ##

str_split(baseball$Name,", ")[1:2]

## Okay, so each element in this list is a character vector where 
## everything to the left of the delimiter is considered the player's 
## last name and everything to the right of the delimiter is considered
## the player's first name ##

## We can use lapply and do.call to create a two-column dataframe and then append it
## to the baseball df ##

player_name_list <- lapply(str_split(baseball$Name,", "),FUN=function(x){
  
  tibble(FirstName = x[2],LastName = x[1])
  
})

player_name_list[1:2]

player_name_df <- do.call('rbind',player_name_list)

player_name_df[1:2,]

## Okay, so now we can append these two columns to the existing baseball df ##

baseball2 <- dplyr::bind_cols(baseball,player_name_df)

## What if we wanted to do the opposite? What if we wanted to take the individual 
## columns and concatenate them together into one column? ##

## A couple of different methods are available to us: ##

## (1) stringr::str_c ##

baseball2$Player_Name <- str_c(baseball2$FirstName,baseball2$LastName,sep=" ")

baseball2[1:3,c("FirstName","LastName","Player_Name")]

## (2) paste ##

baseball2$Player_Name2 <- paste(baseball2$FirstName,baseball2$LastName,sep=" ")

baseball2[1:3,c("Player_Name","Player_Name2")]

## Read in Agents Data ##

agents <- readxl::read_xlsx('agents.xlsx')

## Examine Data ##

View(agents)

## Here we have names again in different columns. Let's concatenate them together 
## in the form of: lastname, firstname middlename ##

agents$agent_name <- str_c(agents$LastName,", ",agents$FirstName," ",
                           agents$MiddleName,sep="")

agents$agent_name2 <- paste(agents$LastName,", ",agents$FirstName," ",
                           agents$MiddleName,sep="")

View(agents)

## Well what if we wanted just the middle initial instead of the full middle name? ##

## Again, we have a couple of different strategies to help us out! ##

## (1): str_sub ##

dplyr::bind_cols(
  
  agents$MiddleName,
  
  str_sub(agents$MiddleName,start=1,end=1)
  
)

## (2): substr ##

dplyr::bind_cols(
  
  agents$MiddleName,
  
  substr(agents$MiddleName,1,1)
  
)

## Now, suppose that we just want to know whether a particular substring of text is
## within a full string. We don't need to substitute or do anything like that

## Here we can use str_detect! ##

## Let's say within the following string, we want to know if any of the words 
## contain the letter "e" ##

x <- c("apple", "banana", "pear")

x

str_detect(x,"e")

upper_x <- str_to_upper(x)

lower_x <- str_to_lower(x)

## Let's look at a slightly more complex example from a real project I 
## have been working on for several years ##

transactions <- readxl::read_xlsx("MLB Transactions 2008 - 2019.xlsx",
                                    sheet=2)

transactions[1:5,]

## This is MLB injury data scraped from the MLB website. The data 
## I needed for my analysis is all contained within this string of 
## text. Let's say I want to subset only the Atlanta Braves 
## transactions without creating a new variable ##

bravos <- transactions[
  which(str_detect(transactions$Note,"Atlanta Braves")),]

bravos[1:10,]

## Alternatively ##

bravos2 <- transactions %>%
  dplyr::filter(str_detect(Note,"Atlanta Braves") == TRUE)

bravos2[1:10,]

## Let's say now that we want to subset only those Braves players who were placed
## on the IL/DL in April 2019 ##

# install.packages('lubridate')

transactions$Date <- lubridate::as_date(transactions$Date)

april19 <- transactions %>%
  dplyr::filter(str_detect(Note,"Atlanta Braves") == TRUE &
                  str_detect(Note,"placed") &
                  lubridate::month(Date) == 4 &
                  lubridate::year(Date) == 2019)

april19

## Let's go through a slightly more complicated example using this april19 dataset ##

## How can we parse out player position? We can see that it's always the fourth word ##

## A couple of strategies are at our disposal, but the easiest is probably str_split ##

## Let's look at how str_split splits up the note: ##

str_split(april19$Note," ")

## Okay so we can pretty clearly see that we need the fourth element 
## returned to us from the character vectors, each of which is considered an element
## in the list generated by str_split ##

april19$Position <- sapply(str_split(april19$Note," "),FUN=function(x){
  
  return(x[4])
  
})

april19

## Now, let's take it up a notch. Let's subset the original transactions dataset 
## such that we get both New York Mets and Atlanta Braves players from April 2019
## who were placed on the IL/DL  ##

new_april19 <- transactions %>%
  dplyr::filter((str_detect(Note,"Atlanta Braves") == TRUE | 
                   str_detect(Note,"New York Mets")) &
                  str_detect(Note,"placed") &
                  lubridate::month(Date) == 4 &
                  lubridate::year(Date) == 2019)

new_april19

## Okay, so now how do we extract the position? It's in position 4 for the Braves
## and position 5 for the Mets. We can make use of an ifelse function! ##

new_april19 <- new_april19 %>%
  dplyr::mutate(Position = ifelse(str_detect(Note,"Atlanta Braves") == TRUE,
                                  sapply(str_split(new_april19$Note," "),FUN=function(x){
                                    
                                    return(x[4])
                                    
                                  }),
                                  
                                  sapply(str_split(new_april19$Note," "),FUN=function(x){
                                    
                                    return(x[5])
                                    
                                  })))
                                  
new_april19

## What if we wanted to extract team name without listing out all 30 teams from the April 2019 season
## who had players placed on the IL/DL ##

## Let's break this down into smaller pieces using the new_april19 dataset ##

## We know that at most, each full team name can have 3 words ##

## If the fourth words is "placed" then we know the team is a 3-name team
## If not, then we know it isn't ##

lapply(str_split(new_april19$Note," "),FUN=function(x){
  
  x[1:4]
  
})

## So to get team name, we can use ifelse again! ##

sapply(str_split(new_april19$Note," "),FUN=function(x){
  
  t <- x[1:4]
  
  ifelse(t[4] == 'placed', str_c(t[1],t[2],t[3],sep=" "),
         str_c(t[1],t[2],sep=" "))
  
})

## Okay, let's go ahead with the full analysis! ##

transactions19 <- transactions %>%
  dplyr::filter(lubridate::year(Date) == 2019 &
                  lubridate::month(Date) == 4 &
                  str_detect(Note,"placed") == TRUE) %>%
  dplyr::mutate(Team = sapply(str_split(Note," "),FUN=function(x){
    
    t <- x[1:4]
    
    ifelse(t[4] == 'placed', str_c(t[1],t[2],t[3],sep=" "),
           str_c(t[1],t[2],sep=" "))
    
  }))

transactions19[1:5,]

table(transactions19$Team)

length(unique(transactions19$Team))
