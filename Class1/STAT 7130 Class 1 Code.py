## STAT 7130 - Class 1 Python Code ##

## Import required libraries ##

import pandas as pd
import numpy as np

## Python as a Calculator ##

## Addition ##

add_result = 2 + 2
print(add_result)

## Subtraction ##

subtract_result = 2 - 2
print(subtract_result)

## Multiplication ##

multiply_result = 2 * 2
print(multiply_result)

## Division ##

divide_result = 2 / 2
print(divide_result)

## Raise to a Power ##

power_result = 2 ** 2
print(power_result)

## Save 2 + 2 as "a" ##

a = 2 + 2
print(a)

b = "STAT 7130"
print(b)

## Like R, Python is case sensitive ##

print(B)

## Importing a CSV File using the Pandas library ##

heart = pd.read_csv('HEART.csv')

## Data Exploration/Integrity Check ##
## Using the describe function ##

print(heart.describe())

## Importing an XLSX File ##

esoph = pd.read_excel("esoph.xlsx")

## Isolating a Variable ##

age_at_death = heart['AgeAtDeath']
print(age_at_death)

## Selecting specific values within a variable ##

age_at_start = heart['AgeAtStart']
print(age_at_start[0])

## Find the mean of AgeAtDeath ##

mean_age_at_death = age_at_death.mean()
print(mean_age_at_death)

## Subset last four columns of Heart ##

heart_status = heart[['Chol_Status', 'BP_Status', 'Weight_Status', 'Smoking_Status']]
print(heart_status)

## Subset Overweight Participants ##

heart_status_ow = heart_status[heart_status['Weight_Status'] == 'Overweight']
print(heart_status_ow)

## Checking to make sure the subset worked ##

print(heart_status['Weight_Status'].value_counts())

## Convert Data From Wide to Long with Pandas ##
## You can achieve this with Pandas' melt function ##

## Read in Income by Religion Data ##

relig_income = pd.read_excel('Income by Religion.xlsx')

## Explore Structure ##

View(relig_income)

## Covert to Long ##

long_relig_income = relig_income.melt(id_vars='religion', var_name='income', value_name='count')
View(long_relig_income)
