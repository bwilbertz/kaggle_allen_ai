# TODO: Add comment
# 
# Author: benedikt
###############################################################################





library(data.table)
library(dplyr)
library(httr)
library(hash)
library(rJava)
library(stringr)
library(Matrix)
library(reshape2)

.jinit()
.jaddClassPath('lib/kaggle_stemming-all-1.0-SNAPSHOT.jar')

janalyzer <- .jnew('stemming.ES_Analyzer','xenglish')

.stem <- function(x){ if (!is.character(x)) x <- as.character(x); .jcall(janalyzer, "S", "stem", x)}
.tokenize <- function(x) str_split(str_trim(str_replace_all(x, '_', '')), fixed(" "))

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



load("input/data.RData")
load("data/text2vec.tcm.20160128.RData")

model.name <- 'tcm_50_xenglish'
corpus <- 'secbq2'

dir <- "features/test_final/"
df.in <- df.test.final

tc <- as.matrix(tcm)

words <- rownames(tc)

hw <- hash()

xxx <- lapply(1:length(words), function(i) hw[[words[[i]]]] <- i)

qname <- paste(corpus,'.',model.name,'.RData', sep='')

tokens.question <- lapply(df.in$question, function(str) .tokenize(.stem(str))[[1]])
tokens.a <- lapply(df.in$answerA, function(str) .tokenize(.stem(str))[[1]])
tokens.b <- lapply(df.in$answerB, function(str) .tokenize(.stem(str))[[1]])
tokens.c <- lapply(df.in$answerC, function(str) .tokenize(.stem(str))[[1]])
tokens.d <- lapply(df.in$answerD, function(str) .tokenize(.stem(str))[[1]])

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
dd <- dfl %>% group_by(id) %>% summarise(mm = mean(`coProb.mean.bm25.secbq2.tcm_50_xenglish`, na.rm = T))
dd$mm[which(is.na(dd$mm))] <- 0.000002	# ultimative fallback when all questions are NA (median from train set)

dfl <- dfl %>% left_join(dd, by="id")
ids.na <- which(is.na(dfl[,3]))
dfl[ids.na,3] <- dfl[ids.na,4] 

dfl <- dfl %>% select(-mm)

filename <- paste(dir, fileprefix, qname, sep='')

save(dfl, file=filename)




dt <- data.table(i = tcm@i, j = tcm@j, x = tcm@x)
setkey(dt, i, j)

words <- rownames(tcm)

hw <- hash()

xxx <- lapply(1:length(words), function(i) if(nchar(words[[i]]) > 0) hw[[words[[i]]]] <- i)

nWords <- nrow(tcm)

bm25 <- F


pmi.a <- lapply(1:length(tokens.question), function(i) tcm.pmi.dt(dt, tokens.question[[i]], tokens.a[[i]], hw, bm25, nWords))
pmi.b <- lapply(1:length(tokens.question), function(i) tcm.pmi.dt(dt, tokens.question[[i]], tokens.b[[i]], hw, bm25, nWords))
pmi.c <- lapply(1:length(tokens.question), function(i) tcm.pmi.dt(dt, tokens.question[[i]], tokens.c[[i]], hw, bm25, nWords))
pmi.d <- lapply(1:length(tokens.question), function(i) tcm.pmi.dt(dt, tokens.question[[i]], tokens.d[[i]], hw, bm25, nWords))

##
## log plain PMI
##
prefix <- "lpmi.mean."
fileprefix <- "df.lpmi.mean."

p.a <- sapply(pmi.a, function(x) mean(log(1+x)))
p.b <- sapply(pmi.b, function(x) mean(log(1+x)))
p.c <- sapply(pmi.c, function(x) mean(log(1+x)))
p.d <- sapply(pmi.d, function(x) mean(log(1+x)))

df.f <- df.in %>% select(id)
df.f[[paste(prefix,corpus,'.',model.name,'.1', sep='')]] <- p.a
df.f[[paste(prefix,corpus,'.',model.name,'.2', sep='')]] <- p.b
df.f[[paste(prefix,corpus,'.',model.name,'.3', sep='')]] <- p.c
df.f[[paste(prefix,corpus,'.',model.name,'.4', sep='')]] <- p.d

dfl <- .f.melt(df.f, 2:5)

filename <- paste(dir, fileprefix, qname, sep='')

save(dfl, file=filename)

##
## log median
##
prefix <- "lpmi.median."
fileprefix <- "df.lpmi.median."

p.a <- sapply(pmi.a, function(x) median(log(1+x)))
p.b <- sapply(pmi.b, function(x) median(log(1+x)))
p.c <- sapply(pmi.c, function(x) median(log(1+x)))
p.d <- sapply(pmi.d, function(x) median(log(1+x)))

df.f <- df.in %>% select(id)
df.f[[paste(prefix,corpus,'.',model.name,'.1', sep='')]] <- p.a
df.f[[paste(prefix,corpus,'.',model.name,'.2', sep='')]] <- p.b
df.f[[paste(prefix,corpus,'.',model.name,'.3', sep='')]] <- p.c
df.f[[paste(prefix,corpus,'.',model.name,'.4', sep='')]] <- p.d

dfl <- .f.melt(df.f, 2:5)

filename <- paste(dir, fileprefix, qname, sep='')

save(dfl, file=filename)


##
## log q25
##
prefix <- "lpmi.q25."
fileprefix <- "df.lpmi.q25."

p.a <- sapply(pmi.a, function(x) quantile(log(1+x), probs=0.25))
p.b <- sapply(pmi.b, function(x) quantile(log(1+x), probs=0.25))
p.c <- sapply(pmi.c, function(x) quantile(log(1+x), probs=0.25))
p.d <- sapply(pmi.d, function(x) quantile(log(1+x), probs=0.25))

df.f <- df.in %>% select(id)
df.f[[paste(prefix,corpus,'.',model.name,'.1', sep='')]] <- p.a
df.f[[paste(prefix,corpus,'.',model.name,'.2', sep='')]] <- p.b
df.f[[paste(prefix,corpus,'.',model.name,'.3', sep='')]] <- p.c
df.f[[paste(prefix,corpus,'.',model.name,'.4', sep='')]] <- p.d

dfl <- .f.melt(df.f, 2:5)

filename <- paste(dir, fileprefix, qname, sep='')

save(dfl, file=filename)


