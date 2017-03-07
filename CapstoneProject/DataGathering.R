###
#
# Capstone Project: Data Gathering 
#
# Author: Antoine Beauchamp. 
# Edited: March 6th, 2017
# Created: February 21st, 2017
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


######################### Function: YelpScrape ###############################
#
# This function is used to scrape review data from Yelp.com
# Arguments: 
# BaseURL: URL to the first page of reviews that you want to scrape

YelpScrape <- function(BaseURL) {


  ReviewCount <- 0 #Counter for the number of reviews. On the Yelp there are 20 per page
#Empty character vectors for the reviews and ratings
  Reviews <- character(0) 
  Ratings <- character(0)
  Dates <- character(0)
  PrevRev <- character(0)
  flag <- 1

  #Now let's iterate over the different Yelp review pages and scrape the data. 
  while(flag==1){
  
    #Yelp URL for the given review page
    page_url <- paste(BaseURL,"?start=",as.character(ReviewCount),sep="")
  
    #Scrape the reviews and ratings from the current URL
    ReviewsNew <- read_html(page_url) %>% html_nodes(".review-content p") %>% html_text
    RatingsNew <- read_html(page_url) %>% html_nodes(".rating-large") %>% html_attr("title")
    DatesNew <- read_html(page_url) %>% html_nodes(".biz-rating-large .rating-qualifier") %>% html_text()
    PrevRevNew <- read_html(page_url) %>% html_nodes(".biz-rating-large .rating-qualifier") %>% as.character()
    
    print(paste("Scraping Yelp page",ceiling(ReviewCount/20)))
    
    #Append new reviews/ratings to existing vectors
    Reviews <- c(Reviews,ReviewsNew)
    Ratings <- c(Ratings,RatingsNew)
    Dates <- c(Dates, DatesNew)
    PrevRev <- c(PrevRev,PrevRevNew)
    
    #Increment the review counter to move to the next page in the following iteration
    ReviewCount=ReviewCount +length(ReviewsNew)
    
    #Loop ending condition
    flag <- if(length(ReviewsNew)==0){0} else {1}
    
  }

 return(list("Reviews"=Reviews, "Ratings"=Ratings, "Dates"=Dates, "PrevRev"=PrevRev))

}

#The discrepancy in values between Reviews and Ratings is arising because the 
#ratings scrape is picking up so-called "previous reviews" as well as updated ones, whereas the 
# reviews scape is only picking up new reviews. 

#Alright well it seems I have the broad strokes for this.
#I just need to figure out what to do with this discrepancy. 


######################### Function: OpenTableScrape ###############################
#
# This function is used to scrape review data from opentable.com
# Arguments: 
# BaseURL: URL to the review page from open table. Note that the URL must end with &page= 
# without specify the page number. This is done in the function.


OpenTableScrape <- function(BaseURL) {

  # Parameters
  ReviewCount <- 1
  Reviews <- character(0)
  Ratings <- character(0)
  Dates <- character(0)
  flag <- 1
  
  while(flag==1) {
  
    #Get URL for current page
    page_url <- paste(BaseURL,as.character(ReviewCount),sep="")
    
    #Obtain ratings/reviews from page
    ReviewsNew <- read_html(page_url) %>% html_nodes("#reviews-results .review-content") %>% html_text
    RatingsNew <- read_html(page_url) %>% html_nodes("#reviews-results .filled") %>% html_attr("title")
    DatesNew <- read_html(page_url) %>% html_nodes(".review-meta-separator+ .color-light") %>% html_text()
    
    #Append ratings/reviews
    Reviews <- c(Reviews,ReviewsNew)
    Ratings <- c(Ratings,RatingsNew)  
    Dates <- c(Dates,DatesNew)
    
    print(paste("Scraping OpenTable page",ReviewCount))
    
    #Increment counter
    ReviewCount <- ReviewCount+1
    
    #This condition checks whether we have reached the end of the reviews
    flag <- if(length(ReviewsNew)==0){0} else {1}
    
  }

  return(list("Reviews"=Reviews, "Ratings"=Ratings, "Dates"=Dates))
}

#I ran into a problem here where that selectorgadget was extracting different <p></p> blocks 
# as separate nodes, using the CSS selector "#reviews-results p". When a user hits Enter in the review, this creates a separate
# paragraph, rather than keeping it within one block. 
# I might be able to get around this by using grepl and finding the last instance 
# of </p>

#The following seems to work, although it also grabs a bunch of html. But I can widdle that
#down with grepl() later on. 
#ReviewsNew <- read_html(page_url) %>% html_nodes("#reviews-results .review-content") 


#Note: Could also extract Date information. This seems to be associated with the selector ".review-meta-separator+ .color-light"






######################### Function: TripAdScrape ###############################
#
# This function is used to scrape review data from Trip Advisor
# Arguments:
# LandingURL: This is the URL for the landing page of the restaurant you want to scrape for. 
# It will be used to link to the full review pages

#The URL change from page to page isn't obvious for this one. But I might be able to get around
#it since the link to the next page is actually contained in the HTML file.

#In this case the page number link data from the HTML. What I want to do is find the page 
#number for the next page. This is under "data-page-number=\"2\" .
#The link to the page is subsequently under "href\"/ShowUserReviews..." "
# This is coded in the following while loop. 

TripAdScrape <- function(LandingURL) {


  #This gets the links to the review page, which are embedded in the review titles
  ReviewTitleLink <- read_html(LandingURL) %>% html_nodes(".quote a") %>% html_attr("href")
  
  #The base URL to the first review page is
  BaseURL <- paste("https://www.tripadvisor.ca",ReviewTitleLink[1],sep="")
  
  #Set parameters for data scraping. 
  ReviewCount <- 1
  Reviews <- character(0)
  Ratings <- character(0)
  Dates1 <- character(0)
  Dates2 <- character(0)
  flag <- 1 
  
  while(flag==1){
  
    print(paste("Scraping Trip Advisor page",ReviewCount))
    
    #For the first page, the URL we want to use is jsut the base URL. For subsequent
    #iterations, we want to grab the hyperlink to the new page from the page links 
    #in the previous page. E.g. page 1 carries a link to page 2 in its HTML. 
    if(ReviewCount == 1){
      page_url <- BaseURL
    } else {
      
      #Grab the page numbers for the links
      pagenum <- read_html(page_url) %>% html_nodes(".pageNum") %>% html_attr("data-page-number") %>% as.numeric()
      #Grab the hyperlinks for the following pages
      hyperlink <- read_html(page_url) %>% html_nodes(".pageNum") %>% html_attr("href") %>% as.character()
      
      page_url <- paste("https://www.tripadvisor.ca",hyperlink[pagenum==ReviewCount],sep="")
    
    }
    
    #Read in reviews and ratings from current page
    ReviewsNew <- read_html(page_url) %>% html_nodes("#REVIEWS p") %>% html_text()
    RatingsNew <- read_html(page_url) %>% html_nodes("#REVIEWS .rating_s_fill") %>% html_attr("alt")
    DatesNew1 <- read_html(page_url) %>% html_nodes(".relativeDate") %>% html_attr("title",default=NA_character_)
    DatesNew2 <- read_html(page_url) %>% html_nodes(".ratingDate") %>% html_text()
    
    
    #End loop condition
    flag <- if(length(ReviewsNew)==0){0} else {1}
    
    #Append new reviews/ratings
    Reviews <- c(Reviews, ReviewsNew)
    Ratings <- c(Ratings, RatingsNew)
    Dates1 <- c(Dates1, DatesNew1)
    Dates2 <- c(Dates2, DatesNew2)
    
    #Increment page count
    ReviewCount <- ReviewCount+1  
    
  }

  return(list("Reviews"=Reviews,"Ratings"=Ratings, "Dates1"=Dates1,"Dates2"=Dates2))
}




#One thing to note is that the number of reviews and ratings picked up here is less than
# the number cited on TripAdvisor, but interestingly enough, the last ratings are all in 
# a language other than English. So this isn't an issue. I've got all the English reviews.

######################### Function: ZomatoScrape ###############################

ZomatoScrape <- function(BaseURL) {

  #Set parameters for data scraping. 
  ReviewCount <- 1
  Reviews <- character(0)
  Ratings <- character(0)
  Dates <- character(0)
  flag <- 1 
  
  Reviews <- read_html(BaseURL) %>% html_nodes(".rev-text-expand , .rev-text") %>% html_text()
  Ratings <- read_html(BaseURL) %>% html_nodes(".rev-text-expand div , .rev-text div") %>% html_attr("aria-label")
  Dates <- read_html(BaseURL) %>% html_nodes("time") %>% html_attr("datetime")

  return(list("Reviews"=Reviews,"Ratings"=Ratings, "Dates"=Dates))
    
}

##### NOTES
#
# 1. One thing to note here is that if a review is long, it will have a "read more" option. 
# For whatever reason, reviews of this type are counted twice by the selector tool, once for the
# truncated version, and once for the full version.
# So to work it out, I can jsut grepl for entries with "read more" in it. 
#
# 2. The format in which I've extracted the ratings is such that there will be double the amount
# of data, half of which will be NAs. I can clean this afterwards. 




######################### MAIN ###############################

#######
#
# Data Acquisition
#
#

#Yelp main review page URL
BaseURL_Yelp <- "https://www.yelp.ca/biz/reds-midtown-tavern-toronto-2"

#OpenTable main review page URL
BaseURL_OpenTable <- "https://www.opentable.com/reds-midtown-tavern?covers=2&dateTime=2017-02-22+19%3A00%23reviews&page="

#Trip Advisor landing page
LandingURL_TripAd <- "https://www.tripadvisor.ca/Restaurant_Review-g155019-d5058760-Reviews-Reds_Midtown_Tavern-Toronto_Ontario.html"

#Zomato main review page URL
BaseURL_Zomato <- "https://www.zomato.com/toronto/reds-midtown-tavern-church-and-wellesley/reviews"

#Scrape data from websites
YelpData <- YelpScrape(BaseURL_Yelp)
OpenTableData <- OpenTableScrape(BaseURL_OpenTable)
TripAdData <- TripAdScrape(LandingURL_TripAd)
ZomatoData <- ZomatoScrape(BaseURL_Zomato)

##Cleaning for OpenTable Dates data

#Find all instances of dates in the form "Dined ## days ago"
DatesLogic <- grepl("[0-9]+.*ago",OpenTableData$Dates)

#Subset date info to get the instances matching the above format
DatesTemp <- OpenTableData$Dates[DatesLogic]
DineDate <- character(length(DatesTemp))

#Extract the actual date information for these instances by comparing to 
# the present date
for (i in 1:length(DatesTemp)){
  #Extract the number of days ago that the review was posted.
  dineDay <- regmatches(DatesTemp,regexpr("[0-9]+",DatesTemp)) %>% as.numeric()
  #Grab today's date
  todayDate <- Sys.Date()
  #Subtract the number of days from today's date
  DineDate[i] <- todayDate - dineDay
}

#Replace the date entries with the proper dates. Note that these are not yet formatted 
# as date class
OpenTableData$Dates[DatesLogic] <- DineDate


save(YelpData,OpenTableData,TripAdData,ZomatoData, file="./Data/CapstoneRawData.RData")

#######
#
# Data Cleaning
# 
#


# Clean Yelp data

#Remove additional previous reviews from Yelp data
NoPrevRev <- grepl("has-previous-review",YelpData$PrevRev) == FALSE
YelpData$Ratings <- YelpData$Ratings[NoPrevRev]
YelpData$Dates <- YelpData$Dates[NoPrevRev]

#Create vector to describe the review category as Yelp
YelpVec <- rep("Yelp",length(YelpData$Reviews))

#Combine Yelp data vectors in DF
YelpDF <- data_frame(Reviews=YelpData$Reviews,Ratings=YelpData$Ratings,Dates=YelpData$Dates, Website=YelpVec)


#Clean OpenTable Data
#
#

# Create vector to describe category of OpenTable
OpenTableVec <- rep("OpenTable", length(OpenTableData$Reviews))

#Create OpenTable data frame
OpenTableDF <- data_frame(Reviews=OpenTableData$Reviews, Ratings=OpenTableData$Ratings, Dates=OpenTableData$Dates,Website=OpenTableVec)


#Clean Zomato Data 
#Remove the double value from the ratings, which take on NAs
RatingsNA <- is.na(ZomatoData$Ratings)==FALSE
ZomatoData$Ratings <- ZomatoData$Ratings[RatingsNA]

#Remove double reviews resulting from full review expansion
FullRev <- grepl("read more",ZomatoData$Reviews) == FALSE
ZomatoData$Ratings <- ZomatoData$Ratings[FullRev]
ZomatoData$Reviews <- ZomatoData$Reviews[FullRev]

#Create vector describe website category
ZomatoVec <- rep("Zomato", length(ZomatoData$Reviews))

ZomatoDF <- data_frame(Reviews=ZomatoData$Reviews,Ratings=ZomatoData$Ratings, Dates=ZomatoData$Dates, Website=ZomatoVec)



#Clean TripAdvisor data

#Replace dates of the form "Reviewed ## days ago" with the proper dates
TripAdData$Dates2[grepl("ago",TripAdData$Dates2)] <- TripAdData$Dates1

#Create vector describing website
TripAdVec <- rep("TripAdvisor",length(TripAdData$Reviews))

TripAdDF <- data_frame(Reviews=TripAdData$Reviews,Ratings=TripAdData$Ratings,Dates=TripAdData$Dates2,Website=TripAdVec)


#Merge all data frames
d1 <-full_join(YelpDF,OpenTableDF)
d2 <- full_join(d1,ZomatoDF)
RawDF <- full_join(d2,TripAdDF) %>% group_by(Website)


write_csv(RawDF,"CapstoneRawData.csv")


######################## NOTES ####################################

#Some things I'm noticing by working with CSS selectors. 
#If there's an element labelled <div class="classname">, it can be selected using the node
# ".classname". 
#If, within this <div> block, there exists a paragraph blocks, <p>, we can access that using
# ".classname p". 
#Similarly, if within the <div> block, there exists a hyperlink, <a>, we can access that using
# ".classname a". The actual attribute "href" of the hyperlink can be obtained by then piping 
# the selector to html_attr("href")
#
# So for example, the code block read_html(testhtml) %>% html_nodes(".quote a") %>% html_attr("href")
# will go find something like this: <div class="quote" .... > <a href=" ...">, and will return
# just the hyperlink info. 
#
# We can also go find all the <p> or <a> blocks by just using "p" or "a". But this is less selective. 
# We can do the same with "div" and "span"
#
# Okay so it seems that any HTML format of the type <...> can be accessed simply by typing
# the command. But this will select all items of that sort. So for instance if we want all images
# we would access "img"
#
# There aren't actually that many basic html commands. The complexities come from associating classes
# to different items. <div class="...". for instance. 
#
# I'm not clear on what the hashtag does. 
