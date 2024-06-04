## STAT 7130 Class 3 Code ##

library(tidyverse)

## Read in Flights, Airports, Planes, Weather, and Airlines Dataframes ##

flights <- nycflights13::flights
airports <- nycflights13::airports
planes <- nycflights13::planes
weather <- nycflights13::weather
airlines <- nycflights13::airlines

## Is airports$faa (the airport's FAA code) really a primary key? ##

## Look at faa variable ##

airports$faa[1:5]

## Tidy approach ##

airports |>
  count(faa) |>
  filter(n > 1)

## Are all the rows in weather distinct/unique? ##

weather |>
  select(origin:hour) |>
  distinct() |>
  count()

nrow(weather)

weather |>
  distinct() |>
  count()

nrow(weather)

## Yep! Create Surrogate Key ##

weather <- weather |>
  mutate(ID = row_number(),
         .before=1)

## Simple Inner Join Example ##

x <- tribble(
  ~key,~val_x,
  1, "x1",
  2, "x2",
  3, "x3")

y <- tribble(
  ~key,~val_y,
  1,"y1",
  2,"y2",
  4,"y3"
)

x1 <- tribble(
  ~key1,~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)

x |>
  inner_join(y,by="key")

x1 |>
  inner_join(y,by=c("key1" = "key"))

## Left Join ##

x |> 
  left_join(y,by="key")

y |> 
  left_join(x,by="key")

## Right Join ##

x |> 
  right_join(y,by="key")

y |>
  right_join(x,by="key")

## Full Join ##

x |> 
  full_join(y,by="key")

## Duplicate Key Values ##

## One dataframe has duplicate keys ##

x <- tribble(
  ~key,~val_x,
  1,"x1",
  2,"x2",
  2,"x3",
  1,"x4"
)

y <- tribble(
  ~key,~val_y,
  1,"y1",
  2,"y2"
)

x |>
  left_join(y,by="key")

## Both dataframes have duplicate keys ##

x <- tribble(~key,~val_x,1,"x1",2,"x2",2,"x3",3,"x4")
y <- tribble(~key,~val_y,1,"y1",2,"y2",2,"y3",3,"y4")

x |>
  left_join(y,by="key")

## Join Flights with Destination Airport ##

## Keys don't have same name (faa in airports and dest in flights) ##

flights2 <- flights |>
  left_join(airports |> 
                     select(faa,name),
                   by = c("dest" = "faa"))

## Perform a Natural Join ##

flights2 |>
  left_join(weather)

## Join Player First and Last Name to 2018 Batting Data ##

batting <- Lahman::Batting |>
  filter(yearID == "2018")

people <- Lahman::People

batting1 <- batting |>
  left_join(people |> 
                     select(playerID,nameFirst,nameLast),
            by="playerID")

## Top 10 Destinations ##

top_dest <- flights |>
  count(dest,sort=TRUE) |>
  head(10)

top_dest

flight3 <- flights |>
  semi_join(top_dest)

## Anti-Join Flights with Planes by Tailnumber ##

flights |>
  anti_join(planes,by="tailnum") |>
  count(tailnum,sort=TRUE)

## Remove Filler Words from Jane Austen Books ##

library(janeaustenr)
library(tidytext)

## Don't worry about this part too much right now ##

original_books <- austen_books() |>
  group_by(book) |>
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) |>
  ungroup()

tidy_books <- original_books |>
  tidytext::unnest_tokens(word, text)

## Notice the data format ##

tidy_books

## Count Up Most Common Words ##

tidy_books |>
  count(word,sort=TRUE)

## Anti_Join with stop_words df ##

data("stop_words")

stop_words[1:5,1]

tidy_books |>
  anti_join(stop_words,by="word") |>
  count(word,sort=TRUE)

## Set Operations ##

df1 <- tribble(~x,~y,1,1,2,1)
df2 <- tribble(~x,~y,1,1,1,2)

df1
df2

## Intersect ##

intersect(df1,df2)

## Union ##

union(df1,df2)

## Setdiff ##

setdiff(df1,df2)

## Other Methods for Stacking dfs ##

union_all(df1,df2)

bind_rows(df1,df2)

rbind.data.frame(df1,df2)

bind_cols(df1,df2)
