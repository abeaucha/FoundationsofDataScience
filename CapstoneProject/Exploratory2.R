##########################################
#
# Exploratory2.R
#
# More structured exploratory analysis
#
# Antoine Beauchamp
# Edited: March 21st, 2017
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
WordCloudAnalysis <- function(dataset, use.sentences=TRUE){
  
  #Remove "The" or "the"  from reviews, since the tm_map() function below doesn't do it. 
  dataset$Reviews <- gsub("[Tt]he", "", dataset$Reviews)
  
  if(use.sentences==TRUE){
    dataCorpus <- get_sentences(dataset$Reviews) %>% VectorSource %>% Corpus()
  }
  else {
    #Create corpus from the full text data
    dataCorpus <- dataset$Reviews %>% VectorSource() %>% Corpus()
  }
  
  #Remove punctuation and english stopwords from text data
  dataCorpus <- dataCorpus %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(content_transformer(tolower))
  
  
  
  #Create term document matrix and convert to matrix class.
  TDM <- TermDocumentMatrix(dataCorpus)
  TDM_m <- TDM %>% as.matrix()
  
  #Compute word frequencies from TDM. 
  wFreqs = sort(rowSums(TDM_m), decreasing=TRUE)
  
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
YearAnalysis <- function(df, auxdf, YearVal){
  
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
  print(paste("The mean value of Ratings for", YearVal, "is: ", auxdf$MeanRating[auxdf$Year==YearVal]))
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
  print(paste("The percentage of 'bad' ratings in",YearVal,"is: ",auxdf$ratioBadRating[auxdf$Year==YearVal]))
  print(paste("The percentage of 'good' ratings in",YearVal,"is: ", auxdf$ratioGoodRating[auxdf$Year==YearVal]))
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
library(syuzhet)

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

t1 <- CapstoneDF %>% group_by(Year) %>% summarise(countYear=n())
CapstoneDF <- left_join(CapstoneDF,t1, by="Year")

t2 <- CapstoneDF %>% group_by(YearQuarters) %>% summarise(countQuarters=n())
CapstoneDF <- left_join(CapstoneDF,t2, by="YearQuarters")

t3 <- CapstoneDF %>% group_by(YearMonth) %>% summarise(countMonth=n())
CapstoneDF <- left_join(CapstoneDF,t3, by="YearMonth")

head(CapstoneDF[-1])



############### GLOBAL ANALYSIS #####################

#Plot width and height
wd=10.0
ht=10.0

#What is full mean of ratings? 
mean(CapstoneDF$Ratings)
CapstoneDF$RatingsAvg <- rep(mean(CapstoneDF$Ratings), length(CapstoneDF$Ratings))

#We can actually do all of our summary statistics within ggplot()

#Ratings aggregated by Year
global_p1 <- ggplot(CapstoneDF, aes(x=Year, y=Ratings)) + 
  geom_line(aes(y=RatingsAvg, group=1), linetype="dashed", colour="red", alpha=0.5) +
  stat_summary(aes(size=countYear), fun.y=mean, geom="point") +
  #coord_cartesian(ylim=c(2.5,4.5)) + 
  scale_size_continuous(range=c(3,7)) +
  coord_cartesian(ylim=c(3.0,4.5))
ggsave(filename="./Plots/Global/global_YearRatings.png",global_p1, width=wd, height=ht)

#Ratings aggregated by Year and Quarters
global_p2 <- ggplot(CapstoneDF, aes(x=YearQuarters, y=Ratings,col=Year)) + 
  geom_line(aes(y=RatingsAvg, group=1), linetype="dashed",colour="black", alpha=0.5) +
  stat_summary(aes(size=countQuarters), fun.y=mean, geom="point") + 
  #coord_cartesian(ylim=c(2.5,4.5)) +
  coord_cartesian(ylim=c(3.0,4.5)) +
  scale_size_continuous(range=c(3,9)) +
  theme(axis.text.x =element_text(angle=90))
global_p2
ggsave(filename="./Plots/Global/global_YearQuartersRatings.png",global_p2,width=wd,height=ht)

#  stat_summary(fun.data=mean_sdl, fun.args=list(mult=1), position="identity") 

#Not quite what I want.
#It's plotting a separate line for Year
ggplot(CapstoneDF, aes(x=YearQuarters, y=Ratings)) + 
  stat_summary(fun.y=mean, geom="point")+ 
  stat_summary(aes(x=Year), fun.y=mean, geom="point", colour="red", size=4.0, position="dodge")

#Ratings aggregated by Year and Month
global_p3 <- ggplot(CapstoneDF, aes(x=YearMonth, y=Ratings, col=Year)) +
  geom_line(aes(y=RatingsAvg, group=1), linetype="dashed",colour="black", alpha=0.5) +
  stat_summary(aes(size=countMonth),fun.y=mean, geom="point", alpha=0.8) +
  coord_cartesian(ylim=c(2.5,4.5)) + 
  scale_size_continuous(range=c(3,8)) +
  theme(axis.text.x =element_text(angle=90))
global_p3
ggsave(filename="./Plots/Global/global_YearMonthRatings.png",global_p3, width=wd, height=ht)

#Something I could do here is fit a linear model to see how linear the growth is.


#Ratings distribution
global_p4 <- ggplot(CapstoneDF, aes(x=ceiling(Ratings))) + geom_bar(aes(fill=Website))
ggsave(filename="./Plots/Global/global_RatingsHist.png", global_p4, width=wd, height=ht)

#Website distribution
global_p5<-ggplot(CapstoneDF, aes(Website)) + geom_bar()
global_p5
ggsave(filename="./Plots/Global/global_Website.png", global_p5, width=wd, height=ht)

#Add facet to encode time information.
global_p6 <- ggplot(CapstoneDF, aes(ceiling(Ratings), y=..density..)) +
  geom_histogram(binwidth=1, col="black") +
  facet_grid(.~Year)
global_p6
ggsave(filename="./Plots/Global/global_RatingsYearHist_dens.png", global_p6, width=wd, height=ht)

#Geom bar density plot
#ggplot(CapstoneDF, aes(x=ceiling(Ratings))) + geom_bar(aes(y=..count../sum(..count..)))

global_p7 <- ggplot(CapstoneDF, aes(Website)) + 
  geom_bar() + 
  facet_grid(.~Year) + 
  theme(axis.text.x =element_text(angle=90))
global_p7
ggsave(filename="./Plots/Global/global_WebsiteYearHist.png", global_p7, width=wd, height=ht)

global_p8 <- ggplot(CapstoneDF, aes(ceiling(Ratings))) + geom_bar() + facet_grid(.~Website)
ggsave(filename="./Plots/Global/global_RatingsWebsiteHist.png", global_p8, width=wd, height=ht)


ggplot(CapstoneDF, aes(x=Website, y=ceiling(Ratings), col=Website)) + 
  stat_summary(fun.y=mean, geom="point", size=10)

#Let's build a plot that describes the time series of the percent of individual star ratings

#Percentage time series analysis. 
yearVec <- sort(unique(as.numeric(CapstoneDF$Year)))

#Build temporary data frame and round up Ratings
CapstoneDF_t <- CapstoneDF
CapstoneDF_t$Ratings <- ceiling(CapstoneDF_t$Ratings)

#Build data frames for rating counts by year
d1 <- CapstoneDF_t %>% group_by(Year,Ratings) %>% summarise(countRatings=n())
d2 <- CapstoneDF_t %>% group_by(Year) %>% summarise(countYear=n())

#Join data frames for ratings count and year counts
percentData <- left_join(d1,d2, by="Year")
percentData$Ratings <- ceiling(percentData$Ratings)


#Create observations for instances when no ratings of that number of stars have been observed
vec2 <- NULL
for(i in 1:length(yearVec)){
  print(i)
  vec <- percentData$Ratings %>% subset(percentData$Year == yearVec[i])
  if(!(1 %in% vec)){
    yearCount <- subset(percentData$countYear, percentData$Year==yearVec[i])[1]
    vec2 <- data.frame(Year=as.character(yearVec[i]), Ratings=as.numeric(1), countRatings=as.integer(0), countYear=as.integer(yearCount))
    print(vec2)
    percentData <- (bind_rows(vec2, as.data.frame(percentData)))
  }
}

#Calculate ratings percentages
percentData <- percentData %>% arrange(Ratings) %>%  mutate(percentRatings = countRatings/countYear)
#Select columns
percentData <- percentData %>% select(Year,Ratings,percentRatings, countYear) %>% arrange(Year,Ratings)
#Treat Ratings as ordinal variables
percentData$Ratings <- factor(percentData$Ratings)

#Plot percent of ratings over time for each ratings, coloured by ratings and sized by total number of ratings for that year
#Scatter + line plot
ggplot(percentData, aes(x=Year, y=percentRatings, group=Ratings)) + 
  geom_line(aes(col=Ratings)) + 
  geom_point(aes(col=Ratings, size=countYear))

#Same as above but just scatter
global_p10 <- ggplot(percentData, aes(x=Year, y=percentRatings, group=Ratings)) + 
  #geom_line(aes(col=Ratings)) + 
  geom_point(aes(col=Ratings,  size=countYear))
global_p10
ggsave(filename="./Plots/Global/global_percentRatings.png", global_p10, width=wd, height=ht)
  
#Add linear regression models to the previous scatter plot.
# Trends can be described linearly or quadratically
global_p11 <- global_p10 + 
  geom_smooth(data=subset(percentData, percentData$Ratings==1), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=0.5, alpha=0.1, col="black") +
  geom_smooth(data=subset(percentData, percentData$Ratings==2), method="lm", se=T, formula=y~poly(x,1, raw=TRUE), linetype="dashed", size=0.5, alpha=0.1, col="black") +
  geom_smooth(data=subset(percentData, percentData$Ratings==3), method="lm", se=T, formula=y~poly(x,1, raw=TRUE), linetype="dashed", size=0.5, alpha=0.1, col="black") + 
  geom_smooth(data=subset(percentData, percentData$Ratings==4), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=0.5, alpha=0.1, col="black") + 
  geom_smooth(data=subset(percentData, percentData$Ratings==5), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=0.5, alpha=0.1, col="black")

global_p11  
ggsave(filename="./Plots/Global/global_percentRatings_model.png", global_p11, width=wd, height=ht)
  
#Alternative way of adding models to the plot
global_p10 +
  geom_line(stat="smooth", data=subset(percentData, percentData$Ratings==1), method="lm", se=T, formula=y~poly(x,2), linetype="dashed", size=0.5) +
  geom_line(stat="smooth", data=subset(percentData, percentData$Ratings==2), method="lm", se=T, formula=y~poly(x,3), linetype="dashed", size=0.5) + 
  geom_line(stat="smooth", data=subset(percentData, percentData$Ratings==3), method="lm", se=T, formula=y~poly(x,1), linetype="dashed", size=0.5) + 
  geom_line(stat="smooth", data=subset(percentData, percentData$Ratings==4), method="lm", se=T, formula=y~poly(x,2), linetype="dashed", size=0.5) + 
  geom_line(stat="smooth", data=subset(percentData, percentData$Ratings==5), method="lm", se=T, formula=y~poly(x,2), linetype="dashed", size=0.5)

#Let's check the numbers on our models to see if they're actually reasonable
summary(lm(percentRatings ~ poly(as.numeric(Year),2), data=subset(percentData, percentData$Ratings==1)))
summary(lm(percentRatings ~ poly(as.numeric(Year),1), data=subset(percentData, percentData$Ratings==2)))
summary(lm(percentRatings ~ poly(as.numeric(Year),1), data=subset(percentData, percentData$Ratings==3)))
summary(lm(percentRatings ~ poly(as.numeric(Year),2), data=subset(percentData, percentData$Ratings==4)))
summary(lm(percentRatings ~ poly(as.numeric(Year),2), data=subset(percentData, percentData$Ratings==5)))

#Most R-squared values are quite high, which is good. Notice that R-squared is low for Ratings==3. This is
# because the data has almost no discernible slope. This means that it can be approximated well by using
# the baseline model. R-squared is a measure of how well a function describes the data, compared to the baseline
# model. If the baseline model works fine, R-squared will be close to 0. This is what is happening with Ratings==3. 


# Nice plots. 

# Let's do word analysis. 


#Word analysis
Capstone_wA <- WordCloudAnalysis(CapstoneDF)

#Check what function returns
names(Capstone_wA)

#Let's look at most common words for full data set.
head(Capstone_wA$wordFreq, n=20)
wordcloud(Capstone_wA$Corpus,min.freq=100, max.word=200, random.order=F)

#Now this is the part where Saurabh suggest I do my hypotheses. 

# Obvious one to start with is food. Associations aren't really relevant.
# E.g. the strongest one is "came". 
findAssocs(Capstone_wA$TDM, "reds", 0.20)
findAssocs(Capstone_wA$TDM, "food", .18)
findAssocs(Capstone_wA$TDM, "good", .15)
findAssocs(Capstone_wA$TDM, "great", .10)
findAssocs(Capstone_wA$TDM, "service", 0.15)

#I don't understand how great could have such a high correlation with "blasting". 
#Only one review in the entire data set contains the word "blasting", while 
# 270 reviews contain the word "great". 
#CapstoneDF$Reviews[grepl("[Bb]lasting",CapstoneDF$Reviews)]
#CapstoneDF$Reviews[grepl("awkwardly",CapstoneDF$Reviews)] #Again only one review
#CapstoneDF$Reviews[grepl("college",CapstoneDF$Reviews)] #One review
#CapstoneDF$Reviews[grepl("article",CapstoneDF$Reviews)] #2 reviews
#
# This problem has been fixed by breaking the reviews down into sentences before
# performing the text analysis. This makes the correlations a lot more relevant. 

length(CapstoneDF$Reviews[grepl("[Gg]reat",CapstoneDF$Reviews)]) #270 reviews
length(CapstoneDF$Reviews[grepl("[Gg]reat",CapstoneDF$Reviews)])/length(CapstoneDF$Reviews) #40%

temp <- CapstoneDF
temp$isgreat <- as.numeric(grepl("[Gg]reat",CapstoneDF$Reviews))
glimpse(temp)
ggplot(temp, aes(x=isgreat, y=..count../sum(..count..))) + geom_bar()


#Let's look at what "good" associates with. 
#CapstoneDF$Reviews[grepl("croutons",CapstoneDF$Reviews)] # 2 reviews
#length(CapstoneDF$Reviews[grepl("surprisingly",CapstoneDF$Reviews)]) #6 reviews
#length(CapstoneDF$Reviews[grepl("vinaigrette",CapstoneDF$Reviews)]) #2 reviews
#length(CapstoneDF$Reviews[grepl("[Tt]hursday.* good",CapstoneDF$Reviews)]) #4 reviews

length(CapstoneDF$Reviews[grepl("[Gg]ood",CapstoneDF$Reviews)]) #287 reviews
length(CapstoneDF$Reviews[grepl("[Gg]ood",CapstoneDF$Reviews)])/length(CapstoneDF$Reviews) #43%

#This doesn't make sense to me. 
# Let's see if this works better with good and bad reviews

#Break data into "good" and "bad" reviews
CapstoneDF_bad <- CapstoneDF %>% subset(Ratings <= 3)
CapstoneDF_good <- CapstoneDF %>% subset(Ratings > 3)

#Perform text analytics on good and bad reviews
CapstoneBad_wA <- WordCloudAnalysis(CapstoneDF_bad)
CapstoneGood_wA <- WordCloudAnalysis(CapstoneDF_good)

#Bad review freq. words
head(CapstoneBad_wA$wordFreq,n=10)
wordcloud(CapstoneBad_wA$Corpus, min.freq=30,max.words=200, random.order=F)

findAssocs(CapstoneBad_wA$TDM, "food",0.15)
findAssocs(CapstoneBad_wA$TDM, "service",0.15)
findAssocs(CapstoneBad_wA$TDM, "personality",0.15)
findAssocs(CapstoneBad_wA$TDM, "good",0.15)
findAssocs(CapstoneBad_wA$TDM, "place",0.15)
findAssocs(CapstoneBad_wA$TDM, "time",0.15)
findAssocs(CapstoneBad_wA$TDM, "server",0.15)
findAssocs(CapstoneBad_wA$TDM, "staff",0.20)
findAssocs(CapstoneBad_wA$TDM, "great",0.20)
findAssocs(CapstoneBad_wA$TDM, "reds",0.20)



#Apparently "good" and "great" are showing up in "bad" reviews too. 
# Do these occur in 3 star reviews or in 1 or 2? 
dim(CapstoneDF_bad)[1] #Total of 212 bad reviews
t_good <- CapstoneDF_bad$Ratings[grepl("[Gg]ood",CapstoneDF_bad$Reviews)]
length(t_good) #96 bad reviews contain the word "good"
length(t_good)/212 #That's 45%
table(t_good)

t_great <- CapstoneDF_bad$Ratings[grepl("[Gg]reat",CapstoneDF_bad$Reviews)]
length(t_great) #57 with "great"
length(t_great)/212 #27%
table(t_great)
# Apparently 16 people rated the restaurant as 2 stars and also said "great". 


log <- grepl("[Gg]reat",CapstoneDF_bad$Reviews) & (CapstoneDF_bad$Ratings==2)
CapstoneDF_bad$Reviews[log]
#Okay that makes more sense of things. A lot of it says great in the past tense, 
# or hypothetically, or about another restaurant. 


length(CapstoneDF_bad$Reviews[grepl("food", CapstoneDF_bad$Reviews)])
length(CapstoneDF_bad$Reviews[grepl("[Ss]ervice", CapstoneDF_bad$Reviews)])
length(CapstoneDF_bad$Reviews[grepl("food.*service|service.*food", CapstoneDF_bad$Reviews)])

#Look at random samples. 
# Want to try to identify sentiment regex. 
CapstoneDF_bad_food <-CapstoneDF_bad %>% filter(grepl("[Ff]ood( +[^ ]+ +){1,10}(poor|awful|bland|disgusting|horrible|overcooked)", CapstoneDF_bad$Reviews)) %>% sample_n(10,replace=TRUE)
select(CapstoneDF_bad_food,Ratings,YearMonth, YearQuarters)
CapstoneDF_bad_food$Reviews

CapstoneDF_bad_great <-CapstoneDF_bad %>% filter(grepl("[Gg]reat", CapstoneDF_bad$Reviews)) %>% sample_n(5,replace=TRUE)
select(CapstoneDF_bad_great,Ratings,YearMonth)
CapstoneDF_bad_great$Reviews




#Good review freq. words
head(CapstoneGood_wA$wordFreq,n=10)
wordcloud(CapstoneGood_wA$Corpus, min.freq=50,max.words=200, random.order=F, colors=brewer.pal(8, "Dark2"))

findAssocs(CapstoneGood_wA$TDM, "great", 0.15)
findAssocs(CapstoneGood_wA$TDM, "food",0.15)
findAssocs(CapstoneGood_wA$TDM, "quality",0.20)
findAssocs(CapstoneGood_wA$TDM, "good",0.20)
findAssocs(CapstoneGood_wA$TDM, "service",0.15)
findAssocs(CapstoneGood_wA$TDM, "reds",0.20)
findAssocs(CapstoneGood_wA$TDM, "menu",0.15)
findAssocs(CapstoneGood_wA$TDM, "place",0.15)
findAssocs(CapstoneGood_wA$TDM, "atmosphere",0.13)
findAssocs(CapstoneGood_wA$TDM, "back",0.15)

dim(CapstoneDF_good)[1]
length(CapstoneDF_good$Reviews[grepl("food", CapstoneDF_good$Reviews)])
length(CapstoneDF_good$Reviews[grepl("[Gg]reat", CapstoneDF_good$Reviews)])
length(CapstoneDF_good$Reviews[grepl("[Gg]ood", CapstoneDF_good$Reviews)])
length(CapstoneDF_good$Reviews[grepl("[Ss]ervice", CapstoneDF_good$Reviews)])
length(CapstoneDF_good$Reviews[grepl("[Ss]ervice.*[Ff]ood|[Ff]ood.*[Ss]ervice", CapstoneDF_good$Reviews)])

t_great <- CapstoneDF_good$Ratings[grepl("[Gg]reat",CapstoneDF_good$Reviews)]
table(t_great)/dim(CapstoneDF_good)[1] #Great seems to be used as often in 4 and 5 star reviews. 

t_good <- CapstoneDF_good$Ratings[grepl("[Gg]ood",CapstoneDF_good$Reviews)]
table(t_good)/dim(CapstoneDF_good)[1] #Good is used more often in 4 star reviews





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
#Use tidyr to convert this to the proper format for plotting. 
# Want format similar to percentData df.
ggplot(auxData, aes(x=Year, y=ratioGoodRating)) + 
  geom_point() + 
  geom_point(aes(x=Year, y=ratioBadRating))



######################### 2013 ANALYSIS #############################

YearAnalysis(CapstoneDF, auxData, 2013)


######################### 2014 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2014)



######################### 2015 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2015)

######################### 2016 ANALYSIS #############################

YearAnalysis(CapstoneDF, 2016) 


######################### 2017 ANALYSIS #############################


YearAnalysis(CapstoneDF, 2017)
