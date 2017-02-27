###
#
# Web Scraping Test Script
#
# Author: Antoine Beauchamp. 
# Edited: February 6th, 2017
# Created: February 6th, 2017
#
#

## NOTE: To practice using CSS selectors, go here: https://flukeout.github.io/



library(rvest)

lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

# Select the cast names using selectorgadget on the webpage. 
# We find the selector: #titleCast .itemprop

#Now we use html_nodes() to load this into R

cast <- html_nodes(lego_movie, "#titleCast .itemprop")

length(cast)
cast[1:4]

#Looking at this output, we can see that we'll get twice as many elements as we want. 
# The names of the actors are in the second element of each pair. 
# Apparently the reason for this is that we've selected the table cell and the text inside the table cell 
# when using selectorgadget. 

# We can experiment with selectorgadget to find a better match
cast <- html_nodes(lego_movie, "#titleCast span.itemprop")

length(cast) #This seems better


#We can look at the html directly using html_text() on the given node. 
html_text(cast)

#There we go. 

#The following also seems to give us what we want, from using selectorgadget
test <- html_nodes(lego_movie, ".itemprop .itemprop")
length(test)
html_text(test)


#############
#
# Source: https://blog.rstudio.org/2014/11/24/rvest-easy-web-scraping-with-r/

# Let's experiment some more, start by extracting the rating from IMDB. 
# Using selectorgadget we find the css selector: strong span

rating <- lego_movie %>%  html_node("strong span") %>% html_text() %>% as.numeric()

#Again, the cast:
cast <- lego_movie %>% html_nodes("#titleCast .itemprop span") %>% html_text()
cast

# Somehow the following extract the message board post titles and author name:
lego_movie %>% html_nodes("table") %>% .[[2]] %>% html_table()
#I can't seem to get the selectorgadget to find the table selector though. 

#Another way to do it is like this:
lego_movie %>% html_nodes(".boards a") %>% html_name()


#######
#
# Let's try this for Reds reviews on Yelp.ca
#
#Source: https://www.yelp.ca/biz/reds-midtown-tavern-toronto-2


baseurl_yelp <- "https://www.yelp.ca/biz/reds-midtown-tavern-toronto-2"




read_html(paste(baseurl_yelp, "?start=140", sep=""))

REDS <- read_html("https://www.yelp.ca/biz/reds-midtown-tavern-toronto-2?start=140")
REDS

test <- REDS %>% html_nodes(".reviews h3")

reviews <- REDS %>% html_nodes(".review-content p")
length(reviews)
reviews[1]
reviews[20]

ratings <- REDS %>% html_nodes(".rating-large")
ratings[1]
length(ratings)


REDS_google <- read_html("https://www.google.ca/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=reds+midtown+tavern&lrd=0x882b34b56f688195:0x7748f8e1643e7f3e,1,")

ratings <- REDS_google %>% html_nodes("#reviewSort span")
ratings
#This doesn't seem to work.


REDS_tripad <- read_html("https://www.tripadvisor.ca/Restaurant_Review-g155019-d5058760-Reviews-Reds_Midtown_Tavern-Toronto_Ontario.html")

#I'm trying to extract the full reviews from the website. 
reviews_wrong <- REDS_tripad %>% html_nodes(".partial_entry")
as.character(reviews_wrong[1])

reviews2 <- REDS_tripad %>% html_nodes(".reviewSelector")
as.character(reviews2[2])

#It seems that both of the above only provide the partial reviews. 

REDS_tripad_rev <- read_html("https://www.tripadvisor.ca/ShowUserReviews-g155019-d5058760-r456761860-Reds_Midtown_Tavern-Toronto_Ontario.html#CHECK_RATES_CONT")
reviews_3 <- REDS_tripad_rev %>% html_nodes("#REVIEWS p")
as.character(reviews_3[1])
as.character(reviews_3[2])
as.character(reviews_3[3])

#Okay this works. You have to click on the review to bring you to a new page where the reviews are expanded out. 





