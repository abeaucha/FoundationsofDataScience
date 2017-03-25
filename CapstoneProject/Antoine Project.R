voc<-read_csv("./Data/CapstoneCleanData.csv")
str(voc)
library(plyr)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(stringr)
library(syuzhet)
library(psych)
library(lubridate)
library(jsonlite)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)


# Code for Text Analysis#
glimpse(voc)

comments <- voc$Reviews

#Seems to be removing some strings? I don't think these are present in my reviews though.
#nohandles <- str_replace_all(comments, "@\\w+", "")
#nohandles <- str_replace_all(comments,"[^[:graph:]]", " ")
nohandles <- sapply(comments,function(row) iconv(row, "latin1", "ASCII", sub=""))
#Not sure what previous command does

#s_v <- comments
s_v <- get_sentences(comments)
#Separating the data into sentences appears to be changing my results. 
# The question is whether sentences or full reviews is better? 
# Organizing them as reviews calculates associations within the full review
# Organizaing as sentences calculates associations within sentences, which might be
# more insightful. 

#s_v <- get_sentences(nohandles)

wordCorpus <- Corpus(VectorSource(s_v))
#Remove URLs
#removeURL <- function(x) gsub("https[[:alnum:]]*","",x)
#wordCorpus <- tm_map(wordCorpus, removeURL)
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower)) #transform to lower case
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
#wordCorpus <- tm_map(wordCorpus, removeWords, c("one","get","will","just","make","even","call","also","got","now", "gautam","nitin", "makemytripcom","makemytrip","amp","httpstco","ranveer", "ranvir","alia", "tvc", "heres"))
#wordCorpus <- tm_map(wordCorpus, stripWhitespace)
#wordCorpus <- tm_map(wordCorpus, PlainTextDocument)

##########################Word Clouds#################
wordcloud(wordCorpus, min.freq = 100, scale = c(4,0.5),
          max.word=500, random.order=F, colors=brewer.pal(8, "Dark2"))

#pal <- brewer.pal(9,"YlGnBu")
#pal <- pal[-(1:4)]
#set.seed(123)
#wordcloud(words = wordCorpus, scale=c(4,.8), max.words=800, random.order=FALSE,
         # rot.per=0.35, use.r.layout=FALSE, colors=pal)

###########################################################

dfTDM <- TermDocumentMatrix(wordCorpus)
findAssocs(dfTDM, "reds", .20)
findAssocs(dfTDM, "food", 0.18)
findAssocs(dfTDM, "friendly", .15)
findAssocs(dfTDM, "service", .15)
findAssocs(dfTDM, "wine", .15)
findAssocs(dfTDM, "lunch", .10)
findAssocs(dfTDM, "dinner", .10)
findAssocs(dfTDM, "menu", .10)
findAssocs(dfTDM, "table", .20)
findAssocs(dfTDM, "night", .10)
findAssocs(dfTDM, "thursday", .10)
findAssocs(dfTDM, "back", .15)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('(?![<>+])[[:punct:]]', '', sentence, perl=TRUE)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    
    #Error handling for tolower conversion
    tryTolower = function(x){
      y = NA
      try_error <- tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
        y = tolower(x)
      return(y)
    }
    sentence = sapply(sentence, tryTolower)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos.words <- scan("positive-words.txt",what='character', comment.char=';', fileEncoding="utf-8")
neg.words <- scan("negative-words.txt",what='character', comment.char=';', fileEncoding="utf-8")

scores <- score.sentiment(s_v, pos.words, neg.words, .progress='text')

mean(scores$score)
hist(scores$score, col = 'light blue', main = "Histogram of Sentiment Score",
     xlab = "Sentiment Score")

# Easy way to do a sentiment analysis. 
# Seem to have no access to words though. This is very vague. 
#
comments_sentiment <- get_nrc_sentiment(s_v)

sentimentTotals <- data.frame(colSums(comments_sentiment))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ggplot(data = sentimentTotals[1:8,], aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score for All Comments")
########################## You may stop here####################################

#afinn_vector <- get_sentiment(s_v, method="bing")


#ft_values <- get_transformed_values(
#  afinn_vector,
#  low_pass_size = 5,
#  x_reverse_len = 100,
#  scale_vals = TRUE,
 # scale_range = FALSE
#)
#plot(
#  ft_values,
#  type ="h",
  #main ="Reviews",
 # xlab = "Narrative Time",
 # ylab = "Emotional Valence",
 # col = "red"
#)

