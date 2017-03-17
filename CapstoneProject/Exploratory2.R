##########################################
#
# Exploratory2.R
#
# More structured exploratory analysis
#
# Antoine Beauchamp
# Edited: March 17th, 2017
# Created: March 16th, 2017
#
#############################


rm(list=ls())


############ FUNCTION DEFINITIONS ##########


# Function: WordCloudAnalysis
#
# This function takes a dataset containing customer reviews and prepares the data for a word cloud
# analysis
# Input: Data frame containing Review variable that has customer reviews
# Output: List containing text analytics properties of reviews such as Corpus, Term Document Matrix, and word freqneucies.
#
WordCloudAnalysis <- function(dataset){
  
  #Remove "The" or "the"  from reviews, since the tm_map() function below doesn't do it. 
  dataset$Reviews <- gsub("[Tt]he", "", dataset$Reviews)
  
  #Create corpus from the text data
  dataCorpus <- dataset$Reviews %>% VectorSource() %>% Corpus()
  
  #Remove punctuation and english stopwords from text data
  dataCorpus <- dataCorpus %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
  
  #Create term document matrix and convert to matrix class.
  TDM <- TermDocumentMatrix(dataCorpus) %>% as.matrix()
  
  #Compute word frequencies from TDM. 
  wFreqs = sort(rowSums(TDM), decreasing=TRUE)
  
  return(list("Corpus" = dataCorpus, "TDM"=TDM, "wordFreq"=wFreqs))
  
}


# Function: YearAnalysis
#
# This function takes a data frame of customer reviews, ratings, over years, 
# computes a number of exploratory graphs, including time series analysis and 
# word cloud text analysis. 
#
# Input: df = Review data frame, YearVal=Year that we want to analyze
# Output: NA. Plots graphs and prints to std.out
#
YearAnalysis <- function(df, YearVal){
  
  #Subset the data to obtain the desired year. 
  data <- df %>% subset(Year==YearVal)
  
  print(paste("Here is a summary of the", YearVal, "data frame:"))
  
  #Some summary output
  cat("\n")
  print(head(data[-1]))
  cat("\n")
  print(str(data))
  
  cat("\n")
  
  # Print mean value of ratings for that year
  print(paste("The mean value of Ratings for", YearVal, "is: ", auxData$MeanRating[auxData$Year==YearVal]))
  cat("\n")
  
  readline(prompt="Press [Enter] to begin plotting.")
  
  #Plot scatter plot of Ratings in the year, aggregated at the level of quarters
  p1 <- ggplot(data,aes(x=YearQuarters, y=Ratings)) + stat_summary(fun.y=mean, geom="point")
  print(p1)
  
  readline(prompt="Press [Enter] to generate the next plot.")
  
  #Plot scatter plot of Ratings in the year, aggregated at the level of months.
  p2 <- ggplot(data,aes(x=YearMonth, y=Ratings, col=YearQuarters)) + stat_summary(fun.y=mean, geom="point")
  print(p2)
  
  readline(prompt="Press [Enter] to generate the next plot.")
  
  #Plot histogram of Ratings, with Website information. 
  p3 <- ggplot(data,aes(ceiling(Ratings))) + geom_bar(aes(fill=Website))
  print(p3)  
  
  readline(prompt="Press [Enter] to generate the next plot.")
  
  #Plot histogram of Rating, w Website info and faceted in quarters
  p4 <- ggplot(data,aes(ceiling(Ratings))) + geom_bar(aes(fill=Website)) + facet_grid(.~YearQuarters)
  print(p4)  
  
  readline(prompt="Press [Enter] to continue.")
  
  #Create subsets of data for "good" and "bad" ratings. 
  dataBad <- data %>% subset(Ratings <= 3)
  dataGood <- data %>% subset(Ratings > 3)
  
  #Print ratios of bad and good ratings for this year.
  print(paste("The percentage of 'bad' ratings in",YearVal,"is: ",auxData$ratioBadRating[auxData$Year==YearVal]))
  print(paste("The percentage of 'good' ratings in",YearVal,"is: ", auxData$ratioGoodRating[auxData$Year==YearVal]))
  cat("\n")
  
  readline(prompt="Press [Enter] to begin text analysis.")
  
  #Run text analysis on full, bad, and good data sets
  data_wA <- WordCloudAnalysis(data)
  dataBad_wA <- WordCloudAnalysis(dataBad)
  dataGood_wA <- WordCloudAnalysis(dataGood)
  
  #Print the most common words from the different data sets. 
  print("Most frequent words from all reviews: ")
  print(head(data_wA$wordFreq))
  cat("\n")
  
  
  print("Most frequent words from 'bad' reviews: ")
  print(head(dataBad_wA$wordFreq))
  cat("\n")
  
  print("Most frequent words from 'good' reviews: ")
  print(head(dataGood_wA$wordFreq))
  cat("\n")
  
  readline(prompt="Press [Enter] to generate word cloud:")
  
  # Plot word clouds
  print("Plotting word cloud for 'bad' reviews." )
  wordcloud(dataBad_wA$Corpus, max.words=200, random.order=F)
  cat("\n")
  
  readline(prompt="Press [Enter] to continue.")
  
  print("Plotting word cloud for 'good' reviews." )
  cat("\n")
  wordcloud(dataGood_wA$Corpus, max.words=200, random.order=F)
  
  
}



####################### DIRECTORY/LIBRARY ###################################


#Set working directory
CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject"
setwd(CapstoneDir)

#Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(ggplot2)

#Read in clean data
CapstoneDF <- read_csv("./Data/CapstoneCleanData.csv")

#Summary of data frame
str(CapstoneDF)
summary(CapstoneDF)
dim(CapstoneDF)


########################## DATA WRANGLING #####################################

#Create Quarters variable
CapstoneDF <- CapstoneDF  %>% mutate(Quarters = quarters.Date(Dates))

#Separate date variable into Year, Month, Day
CapstoneDF <- CapstoneDF %>% separate(Dates, c("Year","Month","Day"))

#Create variable that describes month and year. 
tempdf <- CapstoneDF %>% unite("YearMonth", Year, Month, sep="-")
CapstoneDF$YearMonth <- tempdf$YearMonth

#Create variable that describes quarters and years
tempdf <- CapstoneDF %>% unite("YearQuarters", Year, Quarters, sep="-")
CapstoneDF$YearQuarters <- tempdf$YearQuarters

#Convert Website and Quarters to factor class
CapstoneDF$Website <- factor(CapstoneDF$Website)
CapstoneDF$Quarters <- factor(CapstoneDF$Quarters)

#Take a look at the data
head(CapstoneDF[-1])


############### GLOBAL ANALYSIS #####################

#Plot width and height
wd=10.0
ht=10.0

#What is full mean of ratings? 
mean(CapstoneDF$Ratings)

#We can actually do all of our summary statistics within ggplot()

#Ratings aggregated by Year
global_p1 <- ggplot(CapstoneDF, aes(x=Year, y=Ratings)) + 
  stat_summary(fun.y=mean, geom="point") + 
  coord_cartesian(ylim=c(2.5,4.5))
ggsave(filename="./Plots/global_YearRatings.png",global_p1, width=wd, height=ht)

#Ratings aggregated by Year and Quarters
global_p2 <- ggplot(CapstoneDF, aes(x=YearQuarters, y=Ratings)) + 
  stat_summary(fun.y=mean, geom="point") + 
  coord_cartesian(ylim=c(2.5,4.5))
ggsave(filename="./Plots/global_YearQuartersRatings.png",global_p2,width=wd,height=ht)

#  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), position="identity") 

#Not quite what I want.
#It's plotting a separate line for Year
ggplot(CapstoneDF, aes(x=YearQuarters, y=Ratings)) + 
  stat_summary(fun.y=mean, geom="point")+ 
  stat_summary(aes(x=Year), fun.y=mean, geom="point", colour="red", size=4.0, position="dodge")

#Ratings aggregated by Year and Month
global_p3 <- ggplot(CapstoneDF, aes(x=YearMonth, y=Ratings, col=YearQuarters)) +
  stat_summary(fun.y=mean, geom="point") +
  coord_cartesian(ylim=c(2.5,4.5)) + 
  theme(axis.text.x =element_text(angle=90))
ggsave(filename="./Plots/global_YearMonthRatings.png",global_p3, width=wd, height=ht)

#Something I could do here is fit a linear model to see how linear the growth is.


#Ratings distribution
global_p4 <- ggplot(CapstoneDF, aes(x=ceiling(Ratings))) + geom_bar(aes(fill=Website))
ggsave(filename="./Plots/global_RatingsHist.png", global_p4, width=wd, height=ht)

#Website distribution
global_p5<-ggplot(CapstoneDF, aes(Website)) + geom_bar()
ggsave(filename="./Plots/global_Website.png", global_p5, width=wd, height=ht)

#Add facet to encode time information.
global_p6 <- ggplot(CapstoneDF, aes(ceiling(Ratings), y=..density..)) +
  geom_histogram(binwidth=1, col="black") +
  facet_grid(.~Year)
global_p6
ggsave(filename="./Plots/global_RatingsYearHist_dens.png", global_p6, width=wd, height=ht)

#Geom bar density plot
#ggplot(CapstoneDF, aes(x=ceiling(Ratings))) + geom_bar(aes(y=..count../sum(..count..)))

global_p7 <- ggplot(CapstoneDF, aes(Website)) + 
  geom_bar() + 
  facet_grid(.~Year) + 
  theme(axis.text.x =element_text(angle=90))
ggsave(filename="./Plots/global_WebsiteYearHist.png", global_p7, width=wd, height=ht)

global_p8 <- ggplot(CapstoneDF, aes(ceiling(Ratings))) + geom_bar() + facet_grid(.~Website)
ggsave(filename="./Plots/global_RatingsWebsiteHist.png", global_p8, width=wd, height=ht)


# Nice plots. 
# Let's do word analysis. 


#Word analysis
Capstone_wA <- WordCloudAnalysis(CapstoneDF)

#Check what function returns
names(Capstone_wA)

#Let's look at most common words for full data set.
head(Capstone_wA$wordFreq)
wordcloud(Capstone_wA$Corpus, max.word=200, random.order=F)

#Now this is the part where Saurabh suggest I do my hypotheses. 

#Break data into "good" and "bad" reviews
CapstoneDF_bad <- CapstoneDF %>% subset(Ratings <= 3)
CapstoneDF_good <- CapstoneDF %>% subset(Ratings > 3)

#Perform text analytics on good and bad reviews
CapstoneBad_wA <- WordCloudAnalysis(CapstoneDF_bad)
CapstoneGood_wA <- WordCloudAnalysis(CapstoneDF_good)

#Bad review freq. words
head(CapstoneBad_wA$wordFreq)
wordcloud(CapstoneBad_wA$Corpus, max.words=200, random.order=F)

#Good review freq. words
head(CapstoneGood_wA$wordFreq)
wordcloud(CapstoneGood_wA$Corpus, max.words=200, random.order=F)



## Get some other quantities for data by year

yearVec <- sort(unique(as.numeric(CapstoneDF$Year)))
ratioBad <- numeric(length(yearVec))
ratioGood <- numeric(length(yearVec))
meanVec <- numeric(length(yearVec))
revNum <- numeric(length(yearVec))

auxData <- data_frame(Year=yearVec, MeanRating=meanVec, ReviewNum=revNum, ratioBadRating=ratioBad, ratioGoodRating=ratioGood)

for(i in 1:length(yearVec)){
  data <- CapstoneDF %>% subset(Year==yearVec[i])
  auxData$MeanRating[i] <- mean(data$Ratings)
  auxData$ReviewNum[i] <- dim(data)[1]
  
  dataBad <- data %>% subset(Ratings <= 3)
  dataGood <- data %>% subset(Ratings > 3)
  
  auxData$ratioBadRating[i] <- dim(dataBad)[1]/dim(data)[1]
  auxData$ratioGoodRating[i] <- dim(dataGood)[1]/dim(data)[1]
  
}

auxData

#Look at how ratios of "good" and "bad" reviews have changed over time. 
ggplot(auxData, aes(x=Year, y=ratioGoodRating)) + geom_line() + geom_line(aes(x=Year, y=ratioBadRating))



######################### 2013 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2013)


######################### 2014 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2014)



######################### 2015 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2015)

######################### 2016 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2016) 


######################### 2017 ANALYSIS #############################


YearAnalysis(CapstoneDF, 2017)
