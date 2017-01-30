######### HEADER ###########
#
# Data Wrangling Exercise 2: Dealing with Missing Values
# Springboard: Foundations of Data Science
#
# Author: Antoine Beauchamp
# Edited: January 16th, 2017
# Created: January 16th, 2017
#
#############################


#Clear working environment
rm(list=ls())


library(readxl)
library(readr)
library(dplyr)
library(tidyr)


#Set correct working directory
path_to_wd <- file.path("~","Documents","Work","DataScience","Springboard","FoundationsofDataScience","Section3_DataWrangling","DataWrangling_Exercise2")
setwd(path_to_wd)
rm(path_to_wd)


read_excel("titanic3.xls") %>% write_csv("titanic_original.csv")

titanicdata <- read_csv("titanic_original.csv")

class(titanicdata)
str(titanicdata)
glimpse(titanicdata)
titanicdata


######## Section 1: Port of Embarkation #############

titanic_d1 = titanicdata

titanic_d1$embarked[titanicdata$embarked %>% is.na()] = "S"


titanic_d1 %>% filter(is.na(embarked))
titanic_d1 %>% filter(embarked == '')

######## Section 2: Age #############
# To deal with missing age values, we will use the mean or median of the rest of the values to 
# estimate the data. 

titanic_d2 = titanic_d1

age_mean <- titanic_d1 %>% summarise(mean(age, na.rm=TRUE))
age_median <- titanic_d1 %>% summarise(median(age,na.rm=TRUE))

length(subset(titanic_d1$age, titanic_d1$age %>% is.na()))

titanic_d2$age[titanic_d1$age %>% is.na()] = round(age_mean[[1]])

titanic_d2 %>% filter(is.na(age))
titanic_d2 %>% filter(age=='')
length(subset(titanic_d2$age, titanic_d2$age %>% is.na()))


######## Section 3: Lifeboat #############

titanic_d3 = titanic_d2

length(subset(titanic_d2$boat, titanic_d2$boat %>% is.na()))

titanic_d3$boat[titanic_d2$boat %>% is.na()] = "None"

titanic_d3 %>% filter(is.na(boat))
titanic_d3 %>% filter(boat=='')
length(subset(titanic_d3$boat, titanic_d3$boat %>% is.na()))


######## Section 4: Cabin #############
# 
# I imagine that a missing value for the cabin number would mean that the passengers did 
# not have a cabin. Where were they sleeping? Do research. In that case we could fill this data
# with something like "none" or 0

titanic_clean <- titanic_d3 %>% mutate(has_cabin_number = sapply(titanic_d3$cabin, function (x) {if (is.na(x)==TRUE) {0} else {1}}))

titanic_clean %>% select(cabin, has_cabin_number)


######## Section 5: Write to File #############

write_csv(titanic_clean, "titanic_clean.csv")


