###
#
# Capstone Project: Data Cleaning
#
# Author: Antoine Beauchamp. 
# Edited: March 3rd, 2017
# Created: February 28th, 2017
#
#

CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject"

setwd(CapstoneDir)

rm(list=ls())

#Load required libraries
library(rvest)
library(dplyr)
library(tidyr)
library(readr)


RawDF <- read_csv("CapstoneRawData.csv")

str(RawDF)
summary(RawDF)

head(RawDF$Dates)

#Let's start by cleaning up the dates. 

#First remove any instance where the date is preceded or followed by a newline character
#This will deal with the Yelp dates
RawDF$Dates <- gsub("\n *","",RawDF$Dates)

#Next let's see which dates are in the format ##/##/##, so we know what to clean next
CleanDates <- grepl("^[0-9].*[0-9]$",RawDF$Dates)
#Obtain indices for dates which are not considered "clean"
UncleanInd <- which(CleanDates==FALSE)

#Use the indices to see what the remaining "unclean" dates are
UncleanDates <- RawDF$Dates[UncleanInd]
head(UncleanDates, n=50)

#We see that the first thing that we need to remove is the dates that are 
#followed by "Updated review". 
RawDF$Dates <- gsub("Updated review.*$","", RawDF$Dates)

#The second obvious thing is that a lot of dates begin with "Dined on". 
#These correspond to OpenTable reviews
RawDF$Dates <- gsub("Dined on ","",RawDF$Dates)

#Now let's identify clean dates as dates in the following formats: ##/##/## or [Mm]onth dd, YYYY
CleanDatesRegex <- "(^[0-9].*[0-9]$)|^([Jj]anuary|[Ff]ebruary|[Mm]arch|[Aa]pril|[Mm]ay|[Jj]une|[Jj]uly|[Aa]ugust|[Ss]eptember|[Oo]ctober|[Nn]ovember|[Dd]ecember).*[0-9]"
CleanDates <- grepl(CleanDatesRegex,RawDF$Dates)
UncleanInd <- which(CleanDates==FALSE)
UncleanDates <- RawDF$Dates[UncleanInd]
head(UncleanDates, n=200)

#We can see that, excluding these "clean" dates, we find dates that being with "Reviewed"
# These correspond to TripAdvisor reviews. Let's remove the unnecessary characters. s
RawDF$Dates <- gsub("Reviewed ","",RawDF$Dates)

#All that remains are the Zomato dates. 
#Let's see what they look like
head(subset(RawDF$Dates, RawDF$Website=="Zomato"))

#They are already in a reasonable format so we don't have to remove extra characters yet. 

#Now let's start converting to proper date formats. 

#Begin by getting the Yelp dates for conversion. The Yelp date format is as follows:
head(subset(RawDF$Dates, RawDF$Website=="Yelp"))
#Let's grep for the dates
YelpDateRegex <- grep("^[0-9]+/.*[0-9]$",RawDF$Dates)
#Given the dates, we will express them as date variables
RawDF$Dates[YelpDateRegex] <- RawDF$Dates[YelpDateRegex] %>% as.Date(format="%m/%d/%Y")

#Now do the same with OpenTable. The format is:
head(subset(RawDF$Dates, RawDF$Website=="OpenTable"))
#This will get the OpenTable dates
OpenTableDateRegex <- grep("^([Jj]|[Ff]|[Mm]|[Aa]|[Jj]|[Ss]|[Oo]|[Nn]|[Dd]).+[0-9]+$",RawDF$Dates)
RawDF$Dates[OpenTableDateRegex] <- RawDF$Dates[OpenTableDateRegex] %>% as.Date(format="%B %d, %Y")

head(subset(RawDF$Dates, RawDF$Website=="OpenTable"))

#Next is Trip Advisor. 
head(subset(RawDF$Dates, RawDF$Website=="Trip Advisor"))

TripAdRegex <- grep("^[0-9]+ ([Jj]|[Ff]|[Mm]|[Aa]|[Jj]|[Ss]|[Oo]|[Nn]|[Dd]).+[0-9]+$",RawDF$Dates)
RawDF$Dates[TripAdRegex] <- RawDF$Dates[TripAdRegex] %>% as.Date(format="%d %B %Y")

head(subset(RawDF$Dates, RawDF$Website=="Trip Advisor"))

#Finally, Zomato. 
head(subset(RawDF$Dates, RawDF$Website=="Zomato"))

#Since this is already in POSIX format, we can trivially convert to date
RawDF$Dates[which(RawDF$Website == "Zomato")] <- RawDF$Dates[which(RawDF$Website == "Zomato")] %>% as.Date()

class(RawDF$Dates) <- "Date"

str(RawDF)

# Ratings Cleaning

# Now that we've clean the dates, let's clean up the ratings. 

#What do the Yelp ratings look like? 
head(subset(RawDF$Ratings, RawDF$Website=="Yelp"))

#So we have to get rid of "star rating"
RawDF$Ratings <- gsub("star rating","",RawDF$Ratings)

head(subset(RawDF$Ratings, RawDF$Website=="Yelp"))

#Next OpenTable
head(subset(RawDF$Ratings, RawDF$Website=="OpenTable"))
#Looks good already

#TripAdvisor
head(subset(RawDF$Ratings, RawDF$Website=="TripAdvisor"))

RawDF$Ratings <- gsub("of [0-9] bubbles","",RawDF$Ratings)

head(subset(RawDF$Ratings, RawDF$Website=="TripAdvisor"))


#Zomato
head(subset(RawDF$Ratings, RawDF$Website=="Zomato"))

RawDF$Ratings <- gsub("Rated ","",RawDF$Ratings)

head(subset(RawDF$Ratings, RawDF$Website=="Zomato"))

class(RawDF$Ratings) <- "numeric"


RawDF$Website <- factor(RawDF$Website,order=FALSE,levels=c("Yelp","OpenTable","Zomato","TripAdvisor")) 

str(RawDF)


#Let's remove newline characters from reviews
RawDF$Reviews <- gsub("\n","",RawDF$Reviews)

#Clean up the Zomato reviews a bit by removing the "Rated" at the beginning
subset(RawDF$Reviews,RawDF$Website=="Zomato")

RawDF$Reviews <- gsub(" +Rated *","",RawDF$Reviews)

subset(RawDF$Reviews,RawDF$Website=="Zomato")

write_csv(RawDF, "CapstoneCleanData.csv")

