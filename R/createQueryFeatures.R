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

library(dplyr)
library(reshape2)

source("utils/featureUtils.R", echo=FALSE, encoding="UTF-8")

dir <- "data/test_final/"
fdir <- "features/test_final/"

cols <- 2:6

qnames <- c(
		"simple.And.quizlet.RData",
		"simple.And.quizlet_20160126.RData",
		"simple.And.quizlet.slope10_20160126.RData",
		"simple.And.seeccbq.RData",
		"simple.And.seeccbq.slope10.RData"			
)

qnames.knn <- c(
		"knn.k50.seeccbq.RData",
		"knn.k50.seeccbq.slope10.RData"
)

qnames.explain <- c(
		"explain.simple.seeccbq.RData",
		"explain.simple.seeccbq.slope10.RData"
)

for ( qname in qnames ) {
	
	q.file <- paste("q." , qname, sep='') 
	
	load(paste(dir,q.file, sep=''))
	
	df.f <- data.frame(id = dfq$id)
	
	
	##
	## hits
	##
	prefix <- "hits.abs."
	fileprefix <- "df.hits."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) y$hits$total))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	
	dfl <- .f.melt(df.f, mcols = 3:6)
	
	dfl <- dfl %>% left_join(df.f %>% select(1,2), by="id")
	
	colnames(dfl)[4] <- gsub(".0_", "_question_", colnames(dfl)[4] )
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- gsub(".abs.", ".rel.", colnames(dfl)[3] )
	
	df.hits.simple.and.sw.std <- dfl
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## maxScore
	##
	prefix <- "maxScore.abs."
	fileprefix <- "df.maxScore."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) if(y$hits$total > 0) y$hits$max_score else 0))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	
	dfl <- .f.melt(df.f, mcols = 3:6)
	
	dfl <- dfl %>% left_join(df.f %>% select(1,2), by="id")
	
	colnames(dfl)[4] <- gsub(".0_", "_question_", colnames(dfl)[4] )
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- gsub(".abs.", ".rel.", colnames(dfl)[3] )
	
	df.hits.simple.and.sw.std <- dfl
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## top3Score
	##
	prefix <- "top3Score.abs."
	fileprefix <- "df.top3Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.top.scores(y, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	
	dfl <- .f.melt(df.f, mcols = 3:6)
	
	dfl <- dfl %>% left_join(df.f %>% select(1,2), by="id")
	
	colnames(dfl)[4] <- gsub(".0_", "_question_", colnames(dfl)[4] )
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- gsub(".abs.", ".rel.", colnames(dfl)[3] )
	
	df.hits.simple.and.sw.std <- dfl
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top5Score
	##
	prefix <- "top5Score.abs."
	fileprefix <- "df.top5Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.top.scores(y, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	
	dfl <- .f.melt(df.f, mcols = 3:6)
	
	dfl <- dfl %>% left_join(df.f %>% select(1,2), by="id")
	
	colnames(dfl)[4] <- gsub(".0_", "_question_", colnames(dfl)[4] )
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- gsub(".abs.", ".rel.", colnames(dfl)[3] )
	
	df.hits.simple.and.sw.std <- dfl
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top10Score
	##
	prefix <- "top10Score.abs."
	fileprefix <- "df.top10Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.top.scores(y, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	
	dfl <- .f.melt(df.f, mcols = 3:6)
	
	dfl <- dfl %>% left_join(df.f %>% select(1,2), by="id")
	
	colnames(dfl)[4] <- gsub(".0_", "_question_", colnames(dfl)[4] )
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- gsub(".abs.", ".rel.", colnames(dfl)[3] )
	
	df.hits.simple.and.sw.std <- dfl
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top50Score
	##
	prefix <- "top50Score.abs."
	fileprefix <- "df.top50Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.top.scores(y, top_n = 50)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	
	dfl <- .f.melt(df.f, mcols = 3:6)
	
	dfl <- dfl %>% left_join(df.f %>% select(1,2), by="id")
	
	colnames(dfl)[4] <- gsub(".0_", "_question_", colnames(dfl)[4] )
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- gsub(".abs.", ".rel.", colnames(dfl)[3] )
	
	df.hits.simple.and.sw.std <- dfl
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
}

cols <- 2:5


.sum.max.scores <- function(x, top_n = 10) {
	if(length(x) == 0) return(0)
	
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) x[[i]]$explanation$value))
}


for ( qname in qnames.knn ) {
	
	q.file <- paste("q." , qname, sep='') 
	
	load(paste(dir,q.file, sep=''))
	
	df.f <- data.frame(id = dfq$id)
	
	##
	## maxScore
	##
	prefix <- "maxScore."
	fileprefix <- "df.maxScore."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.max.scores(y, 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## top3Score
	##
	prefix <- "top3Score."
	fileprefix <- "df.top3Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.max.scores(y, 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top5Score
	##
	prefix <- "top5Score."
	fileprefix <- "df.top5Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.max.scores(y, 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top10Score
	##
	prefix <- "top10Score."
	fileprefix <- "df.top10Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.max.scores(y, 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top50Score
	##
	prefix <- "top50Score."
	fileprefix <- "df.top50Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i]]
		
		t <- unlist(lapply(x, function(y) .sum.max.scores(y, 50)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, names(dfq)[cols], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
}



cols <- 2:5

load(paste(fdir, "df.basic.stats.RData", sep=''))
answers <- c('X', 'A', 'B', 'C', 'D')

for ( qname in qnames.explain ) {
	
	q.file <- paste("q." , qname, sep='') 
	
	load(paste(dir,q.file, sep=''))
	
	df.f <- data.frame(id = dfq$id)
	
	##
	## maxScore
	##
	prefix <- "maxScore."
	fileprefix <- "df.maxScore."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.answer(y, top_n = 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.question(y, top_n = 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='')
	
	dff <- dfl %>% select(2,3,4) %>% left_join(dfl.words %>% select(ida, nWords, nWords.question), by="ida")
	dff$perWord <- (dff[,2] + dff[,3])/(dff$nWords + dff$nWords.question)
	dff$aWord <- dff[,2]/dff$nWords
	
	dfl <- dfl %>% left_join(dff %>% select(ida, perWord, aWord), by="ida")
	
	colnames(dfl)[6] <- paste(prefix, 'perWord.', names(dfq)[3], sep='')
	colnames(dfl)[7] <- paste(prefix, 'aWord.', names(dfq)[3], sep='')
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## top3Score
	##
	prefix <- "top3Score."
	fileprefix <- "df.top3Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.answer(y, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.question(y, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	dff <- dfl %>% select(2,3,4) %>% left_join(dfl.words %>% select(ida, nWords, nWords.question), by="ida")
	dff$perWord <- (dff[,2] + dff[,3])/(dff$nWords + dff$nWords.question)
	dff$aWord <- dff[,2]/dff$nWords
	
	dfl <- dfl %>% left_join(dff %>% select(ida, perWord, aWord), by="ida")
	
	colnames(dfl)[6] <- paste(prefix, 'perWord.', names(dfq)[3], sep='')
	colnames(dfl)[7] <- paste(prefix, 'aWord.', names(dfq)[3], sep='')
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	
	##
	## top5Score
	##
	prefix <- "top5Score."
	fileprefix <- "df.top5Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.answer(y, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.question(y, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='')
	
	dff <- dfl %>% select(2,3,4) %>% left_join(dfl.words %>% select(ida, nWords, nWords.question), by="ida")
	dff$perWord <- (dff[,2] + dff[,3])/(dff$nWords + dff$nWords.question)
	dff$aWord <- dff[,2]/dff$nWords
	
	dfl <- dfl %>% left_join(dff %>% select(ida, perWord, aWord), by="ida")
	
	colnames(dfl)[6] <- paste(prefix, 'perWord.', names(dfq)[3], sep='')
	colnames(dfl)[7] <- paste(prefix, 'aWord.', names(dfq)[3], sep='')
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	
	##
	## top10Score
	##
	prefix <- "top10Score."
	fileprefix <- "df.top10Score."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.answer(y, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(x, function(y) .explain.sum.top.scores.question(y, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='')
	
	dff <- dfl %>% select(2,3,4) %>% left_join(dfl.words %>% select(ida, nWords, nWords.question), by="ida")
	dff$perWord <- (dff[,2] + dff[,3])/(dff$nWords + dff$nWords.question)
	dff$aWord <- dff[,2]/dff$nWords
	
	dfl <- dfl %>% left_join(dff %>% select(ida, perWord, aWord), by="ida")
	
	colnames(dfl)[6] <- paste(prefix, 'perWord.', names(dfq)[3], sep='')
	colnames(dfl)[7] <- paste(prefix, 'aWord.', names(dfq)[3], sep='')
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## match ratios
	##
	
	
	##
	## maxScore match ratio
	##
	prefix <- "maxScore.matchRatio."
	fileprefix <- "df.maxScore.matchRatio."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## top3 match ratio
	##
	prefix <- "top3Score.matchRatio."
	fileprefix <- "df.top3Score.matchRatio."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	##
	## top5 match ratio
	##
	prefix <- "top5Score.matchRatio."
	fileprefix <- "df.top5Score.matchRatio."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
	##
	## top10 match ratio
	##
	prefix <- "top10Score.matchRatio."
	fileprefix <- "df.top10Score.matchRatio."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## per Word scores
	##
	prefix <- "maxScore.perWord."
	fileprefix <- "df.maxScore.perWord."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.score.perWord.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 1)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## per Word scores
	##
	prefix <- "top3Score.perWord."
	fileprefix <- "df.top3Score.perWord."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.score.perWord.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 3)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## per Word scores
	##
	prefix <- "top5Score.perWord."
	fileprefix <- "df.top5Score.perWord."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.score.perWord.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 5)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	##
	## per Word scores
	##
	prefix <- "top10Score.perWord."
	fileprefix <- "df.top10Score.perWord."
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		ids <- paste(as.character(df.f$id), answers[i], sep='.')
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.score.perWord.answer(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'answer.', names(dfq)[cols+1], sep='') 
	
	dfl <- .f.melt(df.f, mcols = 2:5)
	
	df.f <- data.frame(id = dfq$id)
	for ( i in cols ) {
		x <- dfq[[i+1]]
		
		t <- unlist(lapply(1:length(x), function(i) .explain.sum.top.match.ratio.question(x[[i]], (dfl.words %>% filter(ida == ids[i]))$nWords.question, top_n = 10)))
		
		df.f[,i] <- t
		
	}
	
	colnames(df.f)[cols] <- paste(prefix, 'question.', names(dfq)[cols+1], sep='') 
	
	dfl.q <- .f.melt(df.f, mcols = 2:5)
	
	dfl <- dfl %>% left_join(dfl.q, by=c("id","ida"))
	
	dfl[,5] <- dfl[,3] / dfl[,4]
	dfl[which(is.nan(dfl[,5])),5] <- 0
	
	colnames(dfl)[5] <- paste(prefix, 'ratio.', names(dfq)[3], sep='') 
	
	filename <- paste(fdir, fileprefix, qname, sep='')
	
	save(dfl, file=filename)
	
	
	
}

