CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject"

setwd(CapstoneDir)

rm(list=ls())


library(readr)
library(dplyr)
library(tm)
library(wordcloud)

CapstoneDF <- read_csv("./Data/CapstoneCleanData.csv")

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
