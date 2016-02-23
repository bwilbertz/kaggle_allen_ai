# 
# Author: Benedikt Wilbertz
###############################################################################



library(tm)
library(RWeka)

library(dplyr)
library(reshape2)

source("utils/featureUtils.R", echo=FALSE, encoding="UTF-8")

load("input/data.RData")

fdir <- "features/test_final/"
df.in <- df.test.final


# number words
df.f <- data.frame(id = df.in$id)
df.f$nWords.question <- sapply(df.in$question, function(x) length(NGramTokenizer(x, Weka_control(min = 1, max = 1))))

df.f$nWords.1 <- sapply(df.in$answerA, function(x) length(NGramTokenizer(x, Weka_control(min = 1, max = 1))))
df.f$nWords.2 <- sapply(df.in$answerB, function(x) length(NGramTokenizer(x, Weka_control(min = 1, max = 1))))
df.f$nWords.3 <- sapply(df.in$answerC, function(x) length(NGramTokenizer(x, Weka_control(min = 1, max = 1))))
df.f$nWords.4 <- sapply(df.in$answerD, function(x) length(NGramTokenizer(x, Weka_control(min = 1, max = 1))))

dfl.words <- .f.melt(df.f, mcols = 3:6)

dfl.words <- dfl.words %>% left_join(df.f %>% select(1,2), by="id")

df.mwords <- dfl.words %>% group_by(id) %>% summarise(mWords = median(nWords), minWords = min(nWords), maxWords = max(nWords))

dfl.words <- dfl.words %>% left_join(df.mwords, by="id")


## sentence end marks
df.f <- data.frame(id = df.in$id)

df.f$nSentences.question <- sapply(df.in$question, function(x) length(grep("[\\.\\?]",unlist(strsplit(gsub("\\.\\.\\.", "", x), '')))))

df.f$nSentences.1 <- sapply(df.in$answerA, function(x) length(grep("[\\.\\?]",unlist(strsplit(gsub("\\.\\.\\.", "", x), '')))))
df.f$nSentences.2 <- sapply(df.in$answerB, function(x) length(grep("[\\.\\?]",unlist(strsplit(gsub("\\.\\.\\.", "", x), '')))))
df.f$nSentences.3 <- sapply(df.in$answerC, function(x) length(grep("[\\.\\?]",unlist(strsplit(gsub("\\.\\.\\.", "", x), '')))))
df.f$nSentences.4 <- sapply(df.in$answerD, function(x) length(grep("[\\.\\?]",unlist(strsplit(gsub("\\.\\.\\.", "", x), '')))))

dfl.sentences <- .f.melt(df.f, mcols = 3:6)

dfl.sentences <- dfl.sentences %>% left_join(df.f %>% select(1,2), by="id")


save(dfl.words, dfl.sentences, file=paste(fdir, "df.basic.stats.RData", sep=''))

