## STAT 7130 - Class 1 Code ##
## R as a Calculator ##

## Add ##

2 + 2

## Subtract ##

2 - 2

## Multiply ##

2*2 

## Divide ##

2/2 

## Raise to a Power ##

2^2

## or ##

2**2

## I can save 2 + 2 as "a" ##

a <- 2 + 2
a

b <- "STAT 7130"

b

## Importing a CSV File ##

## Use the readr package ##

## First, let's install it! ##

install.packages('readr')

## Okay, let's read it in! ##

library(readr)

heart <- read_csv('HEART.csv')

## We can also use: ##

heart <- readr::read_csv('HEART.csv')

## Data Exploration/Integrity Check ##

## With the classic summary function: ##

summary(heart)

## Using dplyr::glimpse: ##

install.packages('dplyr')

library(dplyr)

heart |>
  glimpse()

## Importing an XLSX File ##

## Installing the 'readxl' package ##

install.packages('readxl')

## Importing the 'esoph' dataset ##

library(readxl)

esoph <- read_xlsx("esoph.xlsx")

esoph |>
  glimpse()

## Isolating a Variable: ##

heart$AgeAtDeath

## Selecting specific values within a variable using
## [ ] syntax: ##

heart$AgeAtStart[1]

heart$AgeAtStart[1:5]

## Find the mean of AgeAtDeath ##

mean(heart$AgeAtDeath)

## Summarize AgeAtDeath ##

summary(heart$AgeAtDeath)

## Check the documentation for the mean function ##

?mean

## Try Again Omitting the NA Values ##

mean(age_at_death,na.rm=TRUE)

## Subset last four columns of Heart ##

## All of these methods will yield the same result ##

heart_status <- heart |>
  select(Chol_Status,BP_Status,
                Weight_Status,Smoking_Status)

heart_status1 <- heart[,c("Chol_Status","BP_Status","Weight_Status","Smoking_Status")]

heart_status2 <- heart[,c(14,15,16,17)]

heart_status3 <- heart[,-c(1:13)]

## Subset Overweight Participants ##

heart_status_ow <- heart_status |>
  filter(Weight_Status == 'Overweight')

heart_status_ow1 <- heart_status[which(heart_status$Weight_Status == 'Overweight'),]
                                 
## Checking to Make Sure Subset Worked ##

table(heart_status$Weight_Status)

## Can also use: ##

heart_status |>
  select(Weight) |>
  table()

## Convert Data From Wide to Long with tidyr ##

install.packages('tidyr')

library(tidyr)

relig_income <- tidyr::relig_income

writexl::write_xlsx(relig_income,"Income by Religion.xlsx")

relig_income |>
  head()

long_relig_income <- relig_income |>
  pivot_longer(!religion,names_to="income",
               values_to="count")

long_relig_income |>
  head()
