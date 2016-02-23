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



library(data.table)
library(dplyr)
library(httr)
library(hash)
library(Matrix)
library(reshape2)

library(tm)
library(RWeka)


source("utils/featureUtils.R", echo=FALSE, encoding="UTF-8")
source("utils/vecUtils.R", echo=FALSE, encoding="UTF-8")

tcm.coProb <- function(tcm, t.a, t.b, h, inv = F, bm25 = F) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))
	
	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	
	for (i in seq_along(t.a)) {		
		idx.a <- h[[t.a[i]]]
		for (j in seq_along(t.b)) {
			idx.b <- h[[t.b[j]]]
			
			if ( is.null(idx.a) || is.null(idx.b)) {
				m[j,i] <- NA
				next
			}
			
			if ( idx.a < idx.b ) m[j,i] <- tcm[idx.a, idx.b] else m[j,i] <- tcm[idx.b, idx.a]
			
			if ( bm25 ) m[j,i] <- 2.2*m[j,i] / (1+m[j,i])			
			
			if ( inv ) m[j,i] <- m[j,i] / (sum(tcm[idx.b,])+sum(tcm[,idx.b])) else m[j,i] <- m[j,i] / (sum(tcm[idx.a,])+sum(tcm[,idx.a])) 
			
			#if ( t.a[[i]] == 'be' || t.b[[j]] == 'be') m[j,i] <- NA
		}
	}
	
	m
}



load("input/data_NVAO.RData")
load("data/text2vec.idf.tcm.aws.20151230.RData")

model.name <- 'NVAO_glove_50_500'
corpus <- 'en-si-ck'

dir <- "features/test_final/"
df.in <- df.test.final

tc <- as.matrix(tcm)

words <- rownames(tc)

hw <- hash()

xxx <- lapply(1:length(words), function(i) hw[[words[[i]]]] <- i)

qname <- paste(corpus,'.',model.name,'.RData', sep='')

sanitize.str <- function(str) str

tokens.question <- lapply(df.in$question, function(str) NGramTokenizer(sanitize.str(str), Weka_control(min = 1, max = 1)))
tokens.a <- lapply(df.in$answerA, function(str) NGramTokenizer(sanitize.str(str), Weka_control(min = 1, max = 1)))
tokens.b <- lapply(df.in$answerB, function(str) NGramTokenizer(sanitize.str(str), Weka_control(min = 1, max = 1)))
tokens.c <- lapply(df.in$answerC, function(str) NGramTokenizer(sanitize.str(str), Weka_control(min = 1, max = 1)))
tokens.d <- lapply(df.in$answerD, function(str) NGramTokenizer(sanitize.str(str), Weka_control(min = 1, max = 1)))

inv <- F
cpb.a <- lapply(1:length(tokens.a), function(i) tcm.coProb(tc, tokens.question[[i]], tokens.a[[i]], hw, inv, T))
cpb.b <- lapply(1:length(tokens.b), function(i) tcm.coProb(tc, tokens.question[[i]], tokens.b[[i]], hw, inv, T))
cpb.c <- lapply(1:length(tokens.c), function(i) tcm.coProb(tc, tokens.question[[i]], tokens.c[[i]], hw, inv, T))
cpb.d <- lapply(1:length(tokens.d), function(i) tcm.coProb(tc, tokens.question[[i]], tokens.d[[i]], hw, inv, T))




##
## bm25 coProb
##
prefix <- "coProb.mean.bm25."
fileprefix <- "df.coProb.mean.bm25."

x.a <- sapply(cpb.a, mean, na.rm = T)
x.b <- sapply(cpb.b, mean, na.rm = T)
x.c <- sapply(cpb.c, mean, na.rm = T)
x.d <- sapply(cpb.d, mean, na.rm = T)

df.f <- df.in %>% select(id)

df.f[[paste(prefix,corpus,'.',model.name,'.1', sep='')]] <- x.a
df.f[[paste(prefix,corpus,'.',model.name,'.2', sep='')]] <- x.b
df.f[[paste(prefix,corpus,'.',model.name,'.3', sep='')]] <- x.c
df.f[[paste(prefix,corpus,'.',model.name,'.4', sep='')]] <- x.d

dfl <- .f.melt(df.f, 2:5)

# fix NA issue
dd <- dfl %>% group_by(id) %>% summarise(mm = mean(`coProb.mean.bm25.en-si-ck.NVAO_glove_50_500`, na.rm = T))
dd$mm[which(is.na(dd$mm))] <- 0.00001	# ultimative fallback when all questions are NA (median from train set)

dfl <- dfl %>% left_join(dd, by="id")
ids.na <- which(is.na(dfl[,3]))
dfl[ids.na,3] <- dfl[ids.na,4] 

dfl <- dfl %>% select(-mm)

filename <- paste(dir, fileprefix, qname, sep='')

save(dfl, file=filename)



