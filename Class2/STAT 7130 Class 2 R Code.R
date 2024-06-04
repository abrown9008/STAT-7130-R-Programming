## STAT 7130 Class 2 Code ##

## Attaching the nycflights13 package & flights dataframe ##

library(nycflights13)

flights <- nycflights13::flights

## Filtering only January flights ##

library(tidyverse)

flights |>
  glimpse()

jan_flights <- flights |>
  filter(month == 1)

table(flights$month)

## Non dplyr method using which ##

jan_flights1 <- flights[which(flights$month == 1),]

## SQL Method ##

library(sqldf)

jan_flights2 <- sqldf("SELECT * 
                       FROM flights
                       WHERE month = 1")

## Filter July 4 Flights ##

foj_flights <- flights |>
  filter(month == 7 & day == 4)

## Filter July 4 Flights using which ##

foj_flights2 <- flights[which(flights$month == 7 & flights$day == 4),]

## Filter July 4 Flights using SQL ##

foj_flights3 <- sqldf("SELECT *
                       FROM flights
                       WHERE month = 7 AND day = 4")

## Filtering Holiday Months Flights ##

holiday_cheer <- flights |>
  filter(month == 11 | month == 12)

holiday_cheer1 <- flights |>
  filter(month %in% c(11,12))

## Using which ##

holiday_cheer2 <- flights[which(flights$month == 11 | flights$month == 12),]

## Using SQL ##

holiday_cheer3 <- sqldf("SELECT * 
                         FROM flights
                         WHERE month = 11 OR month = 12")

## 2+ Hour Arrival or Departure Delays

late_flights <- flights |>
  filter(arr_delay >= 120 | dep_delay >= 120)

late_flights1 <- flights |>
  filter(!(arr_delay < 120 & dep_delay < 120))

late_flights2 <- sqldf("SELECT *
                        FROM flights
                        WHERE arr_delay >= 120 OR dep_delay >= 120")

## Numeric/Character Multi-filter ##

new_df <- flights |>
  filter(month == 4 & 
           dest %in% c("ATL","DEN","DFW"))

flights |>
  filter(month == 4 & 
           dest == "ATL" | dest == "DEN" |
                  dest == "DFW")

new_df1 <- flights |>
  filter(month == 4) |>
  filter(dest %in% c("ATL","DEN","DFW"))

new_df2 <- sqldf("SELECT * 
                  FROM flights
                  WHERE month = 4 AND 
                  dest IN ('ATL','DEN','DFW')")

## Ordering Rows using Arrange ##

## The below code will arrange the flights 
## dataframe in descending order

arr_delay_order <- flights |>
  arrange(desc(arr_delay))

## Old School Way Using Order ##

arr_delay_order1 <- flights[order(flights$arr_delay,
                                  decreasing = TRUE),]

## Using SQL ##

arr_delay_order2 <- sqldf("SELECT * 
                           FROM flights
                           ORDER BY arr_delay DESC")

## Finding Distinct Rows ##

## Remove Duplicate Rows from flights: ##

flights |>
  distinct() |>
  count()

flights |>
  unique() |>
  nrow()

## No duplicates!! ##

## Find All Unique Origin & Destination Pairs ##

flights |> 
  distinct(origin, dest)

## Retain all columns ##

flights |> 
  distinct(origin, dest,.keep_all=T)

## Selecting Columns ##

d_a_d <- flights |>
  select(dest,arr_delay,dep_delay)

## Using SQL ##

d_a_d1 <- sqldf("SELECT dest,arr_delay,dep_delay
                 FROM flights")

## Select all columns between year and day (inclusive) ##

y_m_d <- flights |>
  select(year:day)

## Using SQL...not a super easy way to do this, at least that I know of ##
## One solution could involve using the names function to figure out the names
## of all of the columns in between and including your ending columns and then
## coercing that vector of names into a single string separated by commas.
## Would take a little effort, but it's doable. ##

## Select all columns except year, month, and day (inclusive) ##

ae_ymd <- flights |>
  select(!year:day)

## Alternate Approaches ##

y_m_d1 <- flights[,c(4:19)]

ae_ymd1 <- flights[,-c(1:3)]

## Selecting all numeric columns ##

num_cols <- flights |>
  select(where(is.numeric))

flights |>
  select(where(is.character)) |>
  glimpse()

## Renaming Columns (tailnum to tail_num) ##

flights |>
  rename(tail_num = tailnum)

## Creating Average MPH Variable using Mutate ##

flights1 <- flights |>
  select(year:day,
         ends_with("delay"),
         distance,
         air_time) |>
  mutate(speed = (distance/air_time)*60)

## By default, this is appended to the end of the dataframe.
## If we wanted the new column to be leftmost, we can use: ##

flights |>
  select(year:day,
         ends_with("delay"),
         distance,
         air_time) |>
  mutate(speed = (distance/air_time)*60,
         .before=1)

## .before tells us which column the new column should appear
## well before! We can specify column number or name ##

flights |>
  select(year:day,
         ends_with("delay"),
         distance,
         air_time) |>
  mutate(speed = (distance/air_time)*60,
         .before=month)

## Retain only created columns ##

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

## Standardizing Data ##

## Positively Skewed Data ##

set.seed(123)

pos_skew <- tibble(var = rexp(100))

fivenum(pos_skew$var)

hist(pos_skew$var,main="These Data are Positively Skewed",
     xlab="Time-to-Event")

## Standardize It ##

stdize <- pos_skew |>
  mutate(var1 = (var - mean(var))/sd(var))

hist(stdize$var1,main="These Data are Standardized")

## Using Summarize to find Mean of Grouped Data ##

flights |>
  group_by(month,day) |>
  summarize(mean_delay = mean(dep_delay,na.rm=T))

## SQL Method ##

sqldf("SELECT AVG(dep_delay) as mean_delay,month,day
       FROM flights
       GROUP BY month,day")

## Combining dplyr functions ##

flights |>
  group_by(month) |>
  summarize(mean_delay = mean(dep_delay,na.rm=T)) |>
  arrange(desc(mean_delay)) |>
  head(5)

## Similar SQL Functionality ##

sqldf("SELECT month, AVG(dep_delay) as mean_delay
       FROM flights
       GROUP BY month
       ORDER BY mean_delay DESC
       LIMIT 5")

## Counting the Number of Baseball Players Since 1990 with more than 50 HR ##
## By Year ##

bat <- Lahman::Batting

bat |>
  glimpse()

bat |>
  select(playerID) |>
  distinct() |>
  count()

length(unique(bat$playerID))

bat |>
  filter(yearID >= 1990) |>
  select(yearID,HR) |>
  filter(HR > 50) |>
  group_by(yearID) |>
  summarize(count = n()) |>
  arrange(desc(count))

bat |>
  filter(yearID >= 1990) |>
  mutate(Big_HR = if_else(HR >= 50,"Yes","No")) |>
  select(yearID,HR,Big_HR) |>
  group_by(yearID) |>
  count(Big_HR) |>
  filter(Big_HR == "Yes") |>
  select(yearID,n) |>
  rename(Frequency = n)
  

## Counting Distinct Carriers by Destination ##

flights |>
  group_by(dest) |>
  summarize(carriers = n_distinct(carrier)) |>
  arrange(desc(carriers))


