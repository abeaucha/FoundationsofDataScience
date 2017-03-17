##########################################
#
# Exploratory.R
#
# The purpose of this script is to engage in some exploratory descriptive analytics
# of the customer review data
#
# Antoine Beauchamp
# Edited: March 14th, 2017
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
dim(CapstoneDF)




##################### INITIAL EXPLORATION #######################
#
#
#


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



##################### EXPLORATION WITH QUARTERS #######################
#
#
#


#Review data frame
head(CapstoneDF[2:4])

#Create variable with quarters
d_withQuarters <- CapstoneDF %>% mutate(Quarters = quarters.Date(Dates))

#Check format
head(d_withQuarters[-1])

#Separate Date variable into components
d_withQuarters <- d_withQuarters %>% separate(Dates, c("Year","Month","Day"))
head(d_withQuarters[-1])

#Group by year and quarters and average the ratings
d_byQuarters <- d_withQuarters %>% group_by(Year, Quarters) %>% summarise(AvgRatings = mean(Ratings)) %>% unite("YearQuarter", Year, Quarters, sep="-")
head(d_byQuarters)

# Plot average ratings by quarters
ggplot(d_byQuarters, aes(x=YearQuarter, y=AvgRatings)) + geom_point()


##################### EXPLORATION in 2016-2017 #######################
#
#
#

#Create data frame with recent data
d_Recent <- d_withQuarters %>% unite("Dates", Year, Month, Day, sep="-") %>% subset(Dates >= as.Date("20160101", "%Y%m%d"))

dim(d_Recent)
View(d_Recent)

#Okay this gives us about one year's worth of data. 
# Let's dig into this. 


head(d_Recent[-1])

#In addition to our Quarters variable, let's make a variable that describes quarter and year
#This will be important for plotting later. 
test <- d_Recent %>% separate(Dates, c("Year", "Month", "Day"))
test <- test %>% unite("YearQuarters", Year, Quarters, sep="-")
d_Recent$YearQuarters <- test$YearQuarters

head(d_Recent[-1])

d_Recent <- d_Recent %>% separate(Dates, c("Year", "Month", "Day"))

head(d_Recent[-1])

#Let's write an average by month
d_2016_byMonth <- d_Recent %>% group_by(Year,Month) %>% summarise(AvgRatings=mean(Ratings)) %>% unite("YearMonth", Year, Month, sep="-")

#Scatter plot, average aggregated at the level of months
ggplot(d_2016_byMonth, aes(x=YearMonth, y=AvgRatings)) + geom_point()

#Write an averge by quarter
d_2016_byQuarter <- d_Recent %>% group_by(Year, Quarters) %>% summarise(AvgRatings=mean(Ratings)) %>% unite("YearQuarter", Year, Quarters, sep="-")

head(d_2016_byQuarter)

#Scatter plot, average aggregated at quarters
ggplot(d_2016_byQuarter, aes(x=YearQuarter, y=AvgRatings)) + geom_point()

#We see a strong trend in improving reviews through 2016. 
#Is there are a way that I can look at these different levels of granularity on the same plot? 
# Can I use a facet grid? 

#What I could do is plot the average data and facet it by quarters, right? 

head(d_2016_byMonth)
head(d_Recent[-1])

d_2016_byMonth <- d_Recent %>% group_by(Year,Quarters, Month) %>% summarise(AvgRatings=mean(Ratings)) %>%  unite("YearMonth", Year, Month, sep="-")
test <- d_Recent %>% group_by(Year, Quarters, Month) %>% summarise(AvgRatings = mean(Ratings)) %>% unite("YearQuarters", Year, Quarters, sep="-")  

head(d_2016_byMonth)
head(test)

d_2016_byMonth$YearQuarters <- test$YearQuarters
head(d_2016_byMonth)

#Scatter plot, aggregated at month, faceted by quarters. 
ggplot(d_2016_byMonth, aes(x=YearMonth, y=AvgRatings)) + geom_point(aes(col=YearQuarters)) + facet_grid(.~YearQuarters)


head(d_Recent[-1])

#Something I could do is a histogram of reviews of a certain number within
# certain time periods. How many 3s, how many 4s? And so on? 
# Split this up into a facet grid by quarters. 

d_Recent_Rounded <- d_Recent
d_Recent_Rounded$Ratings <- ceiling(d_Recent_Rounded$Ratings)

head(d_Recent_Rounded[-1])

#Histogram (frequency), faceted by quarters
ggplot(d_Recent_Rounded) + geom_histogram(aes(x=Ratings), binwidth = 1) + facet_grid(.~YearQuarters)

#I'm not even really sure how to interpret this data. 
#Maybe the problem is that I'm using a frequency histogram rather than a density histogram. 

#Histogram (density), faceted by quarters
ggplot(d_Recent_Rounded) + geom_histogram(aes(x=Ratings, y=..density..), binwidth = 1) + facet_grid(.~YearQuarters)

#That looks much better. The problem is that the different quarters have different 
# numbers of reviews. 

#Scatterplot, aggregate by month, colour by quarters
ggplot(d_2016_byMonth, aes(x=YearMonth, y=AvgRatings)) + geom_point(aes(col=YearQuarters))

#What's the next step here? 
# Why did our ratings increase? That seems like a good thing to observe. 
# So why were the bad in the first place? The answers to that might be contained
# in the text data. 
# We can stay at the level of granularity of quarters right now

ggplot(d_2016_byQuarter, aes(x=YearQuarter, y=AvgRatings)) + geom_point(col="blue")

#So 2016-Q1 is the lowest one and 2016-Q4 is the highest. 
#Why was 2016-Q1 so bad? Why did it get better? Why is it dropping in this quarter? 

data_2016Q1 <- d_Recent_Rounded %>% subset(YearQuarters == "2016-Q1") %>% unite("Dates", Year, Month, Day, sep="-")
data_2017Q1 <- d_Recent_Rounded %>% subset(YearQuarters == "2017-Q1") %>% unite("Dates", Year, Month, Day, sep="-")


ggplot(d_Recent, aes(x=Ratings, y=..density..)) + 
  geom_bar(data=subset(d_Recent_Rounded, YearQuarters=="2016-Q1"), fill="red", alpha=0.2, binwidth=1) + 
  geom_bar(data=subset(d_Recent_Rounded, YearQuarters=="2017-Q1"), fill="blue", alpha=0.2, binwidth=1)

ggplot(data_2016Q1, aes(x=Ratings)) + geom_bar()

#Should I dig into the text data right away? Or break this up into "bad" reviews and "good" reviews first? 
#Another plot I could make is a layered histogram of Q12016 and Q12017. 

#Remove all instances of "The" because analytics tools below don't seem to remove it
data_2016Q1$Reviews <- gsub("[Tt]he", "", data_2016Q1$Reviews)

#Subset data sets for good and bad reviews. Define bad as being 3 stars or lower.
data_2016Q1_bad <- data_2016Q1 %>% subset(Ratings <= 3)
data_2016Q1_good <- data_2016Q1 %>% subset(Ratings > 3)

head(data_2016Q1_bad[-1])
head(data_2016Q1_good[-1])

#Create corpus for each of the data subsets.
Corpus_2016Q1 <- data_2016Q1$Reviews %>% VectorSource() %>% Corpus
Corpus_2016Q1_bad <- data_2016Q1_bad$Reviews %>% VectorSource() %>% Corpus
Corpus_2016Q1_good <- data_2016Q1_good$Reviews %>% VectorSource() %>% Corpus
inspect(Corpus_2016Q1[1:2])

#Clean up the text data by removing punctuations and stop words
Corpus_2016Q1 <- Corpus_2016Q1 %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
Corpus_2016Q1_bad <- Corpus_2016Q1_bad %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
Corpus_2016Q1_good <- Corpus_2016Q1_good %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
inspect(Corpus_2016Q1[1:2])

#Create term document matrices for subsets
TDM_2016Q1 <- TermDocumentMatrix(Corpus_2016Q1)
TDM_2016Q1_bad <- TermDocumentMatrix(Corpus_2016Q1_bad)
TDM_2016Q1_good <- TermDocumentMatrix(Corpus_2016Q1_good)

#Convert TDM to actual matrix format
TDM_2016Q1_mat <- as.matrix(TDM_2016Q1)
TDM_2016Q1_mat_bad <- as.matrix(TDM_2016Q1_bad)
TDM_2016Q1_mat_good <- as.matrix(TDM_2016Q1_good)

#Get word frequencies for data subsets
wFreqs_2016Q1_bad = sort(rowSums(TDM_2016Q1_mat_bad), decreasing=TRUE)
wFreqs_2016Q1_good = sort(rowSums(TDM_2016Q1_mat_good), decreasing=TRUE)
head(wFreqs_2016Q1_bad, n=10)
head(wFreqs_2016Q1_good, n=10)

#It seems food was mentioned both in the good and bad subsets, as well as good. 
#People seem to have reviewed winterlicious positively. 

wordcloud(Corpus_2016Q1_bad, min.freq=2,scale=c(4,0.5), max.word=200, random.order=F)
wordcloud(Corpus_2016Q1_good, min.freq=2,scale=c(4,0.5), max.word=200, random.order=F)


#Let's look at some word associations.
findAssocs(TDM_2016Q1_bad, "food", .50)
findAssocs(TDM_2016Q1_bad, "place", .67)
findAssocs(TDM_2016Q1_bad, "service", .52)
findAssocs(TDM_2016Q1_bad, "time", .65)
findAssocs(TDM_2016Q1_bad, "meal", .70)
findAssocs(TDM_2016Q1_bad, "good", .80)

#Take a look at some reviews containing certain words
data_2016Q1_bad$Reviews[grepl("[Ff]ood",data_2016Q1_bad$Reviews)]
length(data_2016Q1_bad$Reviews[grepl("[Ff]ood",data_2016Q1_bad$Reviews)])

data_2016Q1_bad$Reviews[grepl("[Ss]ervice",data_2016Q1_bad$Reviews)]
length(data_2016Q1_bad$Reviews[grepl("[Ss]ervice",data_2016Q1_bad$Reviews)])

data_2016Q1_bad$Reviews[grepl("[Tt]ime",data_2016Q1_bad$Reviews)]
length(data_2016Q1_bad$Reviews[grepl("[Tt]ime",data_2016Q1_bad$Reviews)])

data_2016Q1_bad$Reviews[grepl("[Gg]ood|[Gg]reat",data_2016Q1_bad$Reviews)]
length(data_2016Q1_bad$Reviews[grepl("[Gg]ood|[Gg]reat",data_2016Q1_bad$Reviews)])




data_2016Q1_good$Reviews[grepl("[Ff]ood",data_2016Q1_good$Reviews)]
length(data_2016Q1_good$Reviews[grepl("[Ff]ood",data_2016Q1_good$Reviews)])

data_2016Q1_good$Reviews[grepl("[Ww]inter",data_2016Q1_good$Reviews)]
length(data_2016Q1_good$Reviews[grepl("[Ww]inter",data_2016Q1_good$Reviews)])

data_2016Q1_good$Reviews[grepl("[Gg]ood|[Gg]reat",data_2016Q1_good$Reviews)]
length(data_2016Q1_good$Reviews[grepl("[Gg]ood|[Gg]reat",data_2016Q1_good$Reviews)])

data_2016Q1_good$Reviews[grepl("[Ss]ervice",data_2016Q1_good$Reviews)]
length(data_2016Q1_good$Reviews[grepl("[Ss]ervice",data_2016Q1_good$Reviews)])


data_2016Q1_bad$Reviews[grepl("[Mm]usic|[Nn]oise",data_2016Q1_bad$Reviews)]
length(data_2016Q1_bad$Reviews[grepl("[Mm]usic|[Nn]oise",data_2016Q1_bad$Reviews)])

data_2017Q1_bad$Reviews[grepl("[Mm]usic|[Nn]oise",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Mm]usic|[Nn]oise",data_2017Q1_bad$Reviews)])



#I feel like I may need to write this stuff down on paper. What are people liking? What aren't they liking?
# What are the people who reveiwed well saying? And those that aren't? 

### Look at 2017 Q1 now


#Remove all instances of "The" because analytics tools below don't seem to remove it
data_2017Q1$Reviews <- gsub("[Tt]he", "", data_2017Q1$Reviews)

#Subset data sets for good and bad reviews
data_2017Q1_bad <- data_2017Q1 %>% subset(Ratings <= 3)
data_2017Q1_good <- data_2017Q1 %>% subset(Ratings > 3)

head(data_2017Q1_bad[-1])
dim(data_2016Q1_bad)
dim(data_2017Q1_bad)
head(data_2017Q1_good[-1])
dim(data_2017Q1_good)
dim(data_2016Q1_good)

#Create corpus for each of the data subsets.
Corpus_2017Q1 <- data_2017Q1$Reviews %>% VectorSource() %>% Corpus
Corpus_2017Q1_bad <- data_2017Q1_bad$Reviews %>% VectorSource() %>% Corpus
Corpus_2017Q1_good <- data_2017Q1_good$Reviews %>% VectorSource() %>% Corpus
inspect(Corpus_2017Q1[1:2])

#Clean up the text data by removing punctuations and stop words
Corpus_2017Q1 <- Corpus_2017Q1 %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
Corpus_2017Q1_bad <- Corpus_2017Q1_bad %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
Corpus_2017Q1_good <- Corpus_2017Q1_good %>% tm_map(removePunctuation) %>% tm_map(removeWords, stopwords("english"))
inspect(Corpus_2017Q1[1:2])

#Term document matrices
TDM_2017Q1 <- TermDocumentMatrix(Corpus_2017Q1)
TDM_2017Q1_bad <- TermDocumentMatrix(Corpus_2017Q1_bad)
TDM_2017Q1_good <- TermDocumentMatrix(Corpus_2017Q1_good)

#Convert TDM to matrix format
TDM_2017Q1_mat <- as.matrix(TDM_2017Q1)
TDM_2017Q1_mat_bad <- as.matrix(TDM_2017Q1_bad)
TDM_2017Q1_mat_good <- as.matrix(TDM_2017Q1_good)

#Word frequencies
wFreqs_2017Q1_bad = sort(rowSums(TDM_2017Q1_mat_bad), decreasing=TRUE)
wFreqs_2017Q1_good = sort(rowSums(TDM_2017Q1_mat_good), decreasing=TRUE)
head(wFreqs_2017Q1_bad, n=10)
head(wFreqs_2017Q1_good,n=10)

wordcloud(Corpus_2017Q1_bad, min.freq=2,scale=c(4,0.5), max.word=200, random.order=F)
wordcloud(Corpus_2017Q1_good, min.freq=2,scale=c(4,0.5), max.word=200, random.order=F)


data_2017Q1_bad$Reviews[grepl("[Gg]ood",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Gg]ood",data_2017Q1_bad$Reviews)])

data_2017Q1_bad$Reviews[grepl("[Ss]teak(s)?",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Ss]teak(s)?",data_2017Q1_bad$Reviews)])

data_2017Q1_bad$Reviews[grepl("[Ff]ood",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Ff]ood",data_2017Q1_bad$Reviews)])

data_2017Q1_bad$Reviews[grepl("[Ss]ervice",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Ss]ervice",data_2017Q1_bad$Reviews)])

data_2017Q1_bad$Reviews[grepl("[Ww]inter",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Ww]inter",data_2017Q1_bad$Reviews)])



data_2017Q1_good$Reviews[grepl("[Ww]inter",data_2017Q1_good$Reviews)]
length(data_2017Q1_good$Reviews[grepl("[Ww]inter",data_2017Q1_good$Reviews)])

data_2017Q1_good$Reviews[grepl("[Gg]ood|[Gg]reat",data_2017Q1_good$Reviews)]
length(data_2017Q1_good$Reviews[grepl("[Gg]ood|[Gg]reat",data_2017Q1_good$Reviews)])

data_2017Q1_good$Reviews[grepl("[Ss]ervice|[Ss]taff",data_2017Q1_good$Reviews)]
length(data_2017Q1_good$Reviews[grepl("[Ss]ervice|[Ss]taff",data_2017Q1_good$Reviews)])

data_2017Q1_good$Reviews[grepl("[Mm]usic|[Nn]oise",data_2017Q1_good$Reviews)]
length(data_2017Q1_good$Reviews[grepl("[Mm]usic|[Nn]oise",data_2017Q1_good$Reviews)])

data_2017Q1_bad$Reviews[grepl("[Mm]usic|[Nn]oise",data_2017Q1_bad$Reviews)]
length(data_2017Q1_bad$Reviews[grepl("[Mm]usic|[Nn]oise",data_2017Q1_bad$Reviews)])

#What's interesting is that a few people comment that the music is too loud, but it seems that
# that doesn't prevent them from giving a 4 or 5. 




# Let's look at some probabilities. 
length(data_2017Q1_bad$Ratings)/length(data_2017Q1$Ratings) #21%
length(data_2016Q1_bad$Ratings)/length(data_2016Q1$Ratings) #45%

length(data_2017Q1_good$Ratings)/length(data_2017Q1$Ratings) #79%
length(data_2016Q1_good$Ratings)/length(data_2016Q1$Ratings) #55%

length(data_2017Q1$Ratings)
length(data_2016Q1$Ratings)




# It's bothering me that our ratings are lower in march than they've been since October 2016. 
ggplot(d_2016_byMonth, aes(x=YearMonth, y=AvgRatings)) + geom_point(aes(col=YearQuarters))

#There's this weird pattern of alternating rising and falling. 

d_2016_byMonth2 <- d_Recent %>% unite("YearMonth", Year, Month, sep="-")
View(arrange(d_2016_byMonth2[-1], desc(YearMonth)))

d_2016_byMonth2 %>% group_by(YearMonth) %>% summarise(RevCount = n())

#We see that we don't have many statistics for March 2017. Only 2 reviews. So that's not great. 
# The better thing to explore might be why reviews have dipped in January 2017? 

d_March2017 <- d_2016_byMonth2 %>% subset(YearMonth=="2017-03")
d_March2017$Reviews
d_March2017$Ratings

d_Feb2017 <- d_2016_byMonth2 %>% subset(YearMonth=="2017-02")
ggplot(d_Feb2017, aes(Ratings)) + geom_bar()

d_Jan2017 <- d_2016_byMonth2 %>% subset(YearMonth=="2017-01")
ggplot(d_Jan2017, aes(Ratings)) + geom_bar()

ggplot(d_2016_byMonth2, aes(Ratings, fill=YearMonth)) + 
  geom_histogram(data=subset(d_2016_byMonth2, YearMonth=="2017-01"),binwidth=1, alpha=0.2) + 
  geom_histogram(data=subset(d_2016_byMonth2, YearMonth=="2017-02"),binwidth=1, alpha=0.2) 
