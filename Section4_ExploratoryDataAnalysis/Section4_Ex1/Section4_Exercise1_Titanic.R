############### HEADER ###############
# 
# Exploratory Data Analysis: Titanic Exercise
# Springboard: Foundations of Data Science
#
# Author: Antoine Beauchamp
# Edited: January 24th, 2017
# Created: January 20th, 2017
#
########################################

library(ggplot2)

####### DataCamp: Titanic Exercise Code, as is #####################
#
# The following code is exactly what I coded into the console on DataCamp. 
# Comments were added for clarity. 
#

# Note: This will only execute if the appropriate "titanic" dataset is avaliable in your workspace.

# Check out the structure of titanic
str(titanic)

# Use ggplot() to look at the distribution of sexes within the classes of the ship. 
ggplot(titanic, aes(x=factor(Pclass),fill=factor(Sex)))+geom_bar(position="dodge")

# We want to improve on the previous to give us information about the chances of survival, by including a facet grid
ggplot(titanic, aes(x=factor(Pclass),fill=factor(Sex)))+geom_bar(position="dodge") + facet_grid(".~Survived")

# Let's set the jitter we want. 
posn.j <- position_jitter(0.5, 0)

# Finally, we also want to see how survival is related to age and to passenger class. We will map sex onto colour
# rather than the y axis. 
ggplot(titanic, aes(x=factor(Pclass),y=Age,col=factor(Sex)))+geom_jitter(size=3, alpha=0.5, position=posn.j) + facet_grid(".~Survived")




####### Working With titanic_clean.csv Data Set #####################
#
# The following code performs the same analysis as that above, but it uses data
# from titanic_clean.csv, which we generated in Section 3. 
#
#

#Clear working environment. 
rm(list=ls())

#Load additional data wrangling libraries
library(dplyr)
library(readr)

#Set the wd
path_to_wd <- file.path("~","Documents","Work","DataScience","Springboard","FoundationsofDataScience","Section4_ExploratoryDataAnalysis","Section4_Ex1")
setwd(path_to_wd)
rm(path_to_wd)

#Read in the data from titanic_clean.csv
titanic_full <- suppressMessages(read_csv("titanic_clean.csv"))

#Examine the structure of the full data.                             
str(titanic_full)     

#Let's create our own version of the "titanic" data frame using a select() command. 
titanic <- titanic_full %>% select(pclass,survived,sex,age)

#Examine the structure of this reduced data frame. 
glimpse(titanic)
str(titanic)


#Start by plotting passenger class against sex as a bar plot
ggplot(titanic, aes(x=factor(pclass),fill=factor(sex))) + geom_bar(position='dodge')


titanic <- titanic[!is.na(titanic$pclass),]

ggplot(titanic, aes(x=factor(pclass),fill=factor(sex))) + geom_bar(position='dodge')

#Include information about passenger survival by using a facet grid. 
ggplot(titanic, aes(x=factor(pclass),fill=factor(sex))) + geom_bar(position="dodge")+facet_grid(".~survived")

#Finally, include age information by switching to a scatter plot and mapping age to the y axis and sex to the colour scale. 
posn.j <- position_jitter(0.5, 0)
ggplot(titanic, aes(x=factor(pclass),y=age, col=factor(sex))) + geom_jitter(position=posn.j, size=3, alpha=0.5)+facet_grid(".~survived")





