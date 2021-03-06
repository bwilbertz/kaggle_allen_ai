# The MIT License (MIT)
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# 
# Author: Benedikt Wilbertz
###############################################################################


library(elastic)
library(elasticdsl)
library(reshape2)
library(dplyr)

source("utils/featureUtils.R", echo=FALSE, encoding="UTF-8")

load("input/data.RData")

connect()

size <- 50

## train or test
dir <- "data/test_final/"
df.in <- df.test.final


##
## quizlet
##
name.suffix <- ".quizlet" 
index <- "quizlet-english-bm25"

## simple and
filename <- paste(dir, "q.simple.And", name.suffix, ".RData", sep='')

dfq <- .q.simple.and(df.in, index=index, size = size)

save(dfq, file=filename)

##
## new 20160126 version
##
name.suffix <- ".quizlet_20160126"
index <- "quizlet_20160126-english-bm25"


filename <- paste(dir, "q.simple.And", name.suffix, ".RData", sep='')

dfq <- .q.simple.and(df.in, index=index, size = size)

save(dfq, file=filename)


##
## slope 10
##
slope <- 10
name.suffix <- ".quizlet.slope10_20160126"
index <- "quizlet_20160126-english-bm25"

filename <- paste(dir, "q.simple.And", name.suffix, ".RData", sep='')

dfq <- .q.simple.and.slope(df.in, index=index, size = size, answer.slope = slope)

save(dfq, file=filename)


##
## all
##
name.suffix <- ".seeccbq" 
index <- paste("simplewiki-english-bm25-wiki2json-byparagraph",
		"enwiki-english-bm25-wiki2json-byparagraph",
		"enwikibooks-english-bm25-wiki2json-byparagraph",
		"ck12-english-bm25",
		"ck12_lessons-english-bm25",
		"books-english-bm25",
		"quizlet-english-bm25", sep=',')

## simple and
filename <- paste(dir, "q.simple.And", name.suffix, ".RData", sep='')

dfq <- .q.simple.and(df.in, index=index, size = size)

save(dfq, file=filename)

##
## all, slope = 10
##
slope <- 10
name.suffix <- ".seeccbq.slope10" 
index <- paste("simplewiki-english-bm25-wiki2json-byparagraph",
		"enwiki-english-bm25-wiki2json-byparagraph",
		"enwikibooks-english-bm25-wiki2json-byparagraph",
		"ck12-english-bm25",
		"ck12_lessons-english-bm25",
		"books-english-bm25",
		"quizlet-english-bm25", sep=',')

## simple and
filename <- paste(dir, "q.simple.And", name.suffix, ".RData", sep='')

dfq <- .q.simple.and.slope(df.in, index=index, size = size, answer.slope = slope)

save(dfq, file=filename)


##
## knn
##

##
## all
##
k <- 50
name.suffix <- ".seeccbq" 
index <- paste("simplewiki-english-bm25-wiki2json-byparagraph",
		"enwiki-english-bm25-wiki2json-byparagraph",
		"enwikibooks-english-bm25-wiki2json-byparagraph",
		"ck12-english-bm25",
		"ck12_lessons-english-bm25",
		"books-english-bm25",
		"quizlet-english-bm25", sep=',')

## simple and
filename <- paste(dir, paste("q.knn.k",k, sep=''), name.suffix, ".RData", sep='')

dfq <- .q.knn(df.in, index=index, k = k, size = size, answer.as.filter = T)

save(dfq, file=filename)

##
## all,slope10
##
slope <- 10
k <- 50
name.suffix <- ".seeccbq.slope10" 
index <- paste("simplewiki-english-bm25-wiki2json-byparagraph",
		"enwiki-english-bm25-wiki2json-byparagraph",
		"enwikibooks-english-bm25-wiki2json-byparagraph",
		"ck12-english-bm25",
		"ck12_lessons-english-bm25",
		"books-english-bm25",
		"quizlet-english-bm25", sep=',')

## simple and
filename <- paste(dir, paste("q.knn.k",k, sep=''), name.suffix, ".RData", sep='')

dfq <- .q.knn(df.in, index=index, k = k, size = size, answer.as.filter = T, answer.as.phrase = T, answer.slope = slope)

names(dfq) <- gsub("knn.k50","knn.k50.slope10", names(dfq))

save(dfq, file=filename)

##
## explain features
##

##
## all
##
name.suffix <- ".seeccbq" 
index <- paste("simplewiki-english-bm25-wiki2json-byparagraph",
		"enwiki-english-bm25-wiki2json-byparagraph",
		"enwikibooks-english-bm25-wiki2json-byparagraph",
		"ck12-english-bm25",
		"ck12_lessons-english-bm25",
		"books-english-bm25",
		"quizlet-english-bm25", sep=',')

## simple and
filename <- paste(dir, "q.explain.simple", name.suffix, ".RData", sep='')

dfq <- .q.explain.simple(df.in, index=index, size = size)

save(dfq, file=filename)

##
## all, slope = 10
##
slope <- 10
name.suffix <- ".seeccbq.slope10" 
index <- paste("simplewiki-english-bm25-wiki2json-byparagraph",
		"enwiki-english-bm25-wiki2json-byparagraph",
		"enwikibooks-english-bm25-wiki2json-byparagraph",
		"ck12-english-bm25",
		"ck12_lessons-english-bm25",
		"books-english-bm25",
		"quizlet-english-bm25", sep=',')

## simple and
filename <- paste(dir, "q.explain.simple", name.suffix, ".RData", sep='')

dfq <- .q.explain.simple(df.in, index=index, size = size, answer.slope = slope)

save(dfq, file=filename)

