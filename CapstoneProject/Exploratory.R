##########################################
#
# Exploratory.R
#
# The purpose of this script is to engage in some exploratory descriptive analytics
# of the customer review data
#
# Antoine Beauchamp
# Edited: March 9th, 2017
# Created: March 7th, 2017
#
#############################



CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject"

setwd(CapstoneDir)

rm(list=ls())


library(readr)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(ggplot2)

CapstoneDF <- read_csv("./Data/CapstoneCleanData.csv")

str(CapstoneDF)
summary(CapstoneDF)


d_byDay <- CapstoneDF %>% arrange(Dates)

head(d_byDay[2:4])

ggplot(d_byDay, aes(x=Dates, y=Ratings)) + geom_point()

#This isn't good on this time scale. Would be fine on a shorter time scale. 
# Let's lower the granularity. 

d_DateSep <- CapstoneDF %>% separate(Dates, c("Year","Month","Day")) %>% arrange(Year, Month)

head(d_DateSep[2:6],n=20)

View(d_DateSep)

d_byMonth <- d_DateSep %>% group_by(Year,Month) %>% summarise(AvgRatings=mean(Ratings))

head(d_byMonth,n=10)

d_YearMonth <- d_byMonth %>% unite("Dates", Year, Month, sep="-")

head(d_YearMonth)

ggplot(d_YearMonth, aes(x=Dates,y=AvgRatings)) + geom_point()

# Can I group by quarters? 
# First let's group by year and see. 

head(d_DateSep[2:6])

d_byYear <- d_DateSep %>% group_by(Year) %>% summarise(AvgRatings=mean(Ratings))
head(d_byYear)

ggplot(d_byYear, aes(x=Year,y=AvgRatings)) + geom_point()

#Alright let's dive into quarters then. 

head(d_DateSep[2:6])

#Identify quarters as follows: 
#Q1: Jan, Feb, March
#Q2: April, May, June
#Q3: July, Aug, Sept
#Q4: Oct, Nov, Dec.

head(CapstoneDF[2:4])

d_withQuarters <- CapstoneDF %>% mutate(Quarters = quarters.Date(Dates))

head(d_withQuarters[-1])

d_withQuarters <- d_withQuarters %>% separate(Dates, c("Year","Month","Day"))
head(d_withQuarters[-1])

d_byQuarters <- d_withQuarters %>% group_by(Year, Quarters) %>% summarise(AvgRatings = mean(Ratings)) %>% unite("YearQuarter", Year, Quarters, sep="-")
head(d_byQuarters)

ggplot(d_byQuarters, aes(x=YearQuarter, y=AvgRatings)) + geom_point()


d_Recent <- subset(d_withQuarters, d_withQuarters$Dates >= as.Date("20160101", "%Y%m%d"))

dim(d_Recent)
View(d_Recent)

#Okay this gives us about one year's worth of data. 
# Let's dig into this. 

head(d_Recent[-1])

d_2016_byMonth <- d_Recent %>% separate(Dates, c("Year", "Month", "Day")) %>% group_by(Year,Month) %>% summarise(AvgRatings=mean(Ratings)) %>% unite("YearMonth", Year, Month, sep="-")

ggplot(d_2016_byMonth, aes(x=YearMonth, y=AvgRatings)) + geom_point()


d_2016_byQuarter <- d_Recent %>% separate(Dates, c("Year", "Month", "Day")) %>% group_by(Year, Quarters) %>% summarise(AvgRatings=mean(Ratings)) %>% unite("YearQuarter", Year, Quarters, sep="-")

head(d_2016_byQuarter)

ggplot(d_2016_byQuarter, aes(x=YearQuarter, y=AvgRatings)) + geom_point()

#We see a strong trend in improving reviews through 2016. 
#Is there are a way that I can look at these different levels of granularity on the same plot? 
# Can I use a facet grid? 

#What I could do is plot the average data and facet it by quarters, right? 







CapstoneCorpus <- CapstoneDF$Reviews %>% VectorSource() %>% Corpus()
inspect(CapstoneCorpus[1:2])

CapstoneCorpus <- CapstoneCorpus %>% tm_map(removePunctuation)
CapstoneCorpus <- CapstoneCorpus %>% tm_map(removeWords, stopwords("english"))
inspect(CapstoneCorpus[1:2])


TermDocMatrix <- TermDocumentMatrix(CapstoneCorpus)
inspect(TermDocMatrix)

wordmatrix <- as.matrix(TermDocMatrix)

wordfreqs = sort(rowSums(wordmatrix), decreasing=TRUE)
head(wordfreqs)

# create a data frame with words and their frequencies
WordFreqDF = data_frame(word=names(wordfreqs), freq=wordfreqs)

head(WordFreqDF)


wordcloud(CapstoneCorpus, min.freq=2,scale=c(4,0.5), max.word=200, random.order=F)

findAssocs(TermDocMatrix, "good", .20)
findAssocs(TermDocMatrix, "service", .18)
findAssocs(TermDocMatrix,"food",0.20)
findAssocs(TermDocMatrix, "great", .20)
findAssocs(TermDocMatrix, "drinks", .25)
findAssocs(TermDocMatrix, "dinner", .33)
findAssocs(TermDocMatrix, "time", .25)
findAssocs(TermDocMatrix, "atmosphere", .20)
findAssocs(TermDocMatrix, "friendly", .25)
