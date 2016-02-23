# 
# Author: Benedikt Wilbertz
###############################################################################



library(reshape2)
library(dplyr)
library(rJava)
library(stringr)
library(RWeka)
library(FeatureHashing)
library(xgboost)

source("utils/featureUtils.R", echo=FALSE, encoding="UTF-8")

load("input/data.RData")

## train or test
fdir <- "features/test_final/"
df.in <- df.test.final

cols <- (ncol(df.in)-3):ncol(df.in)

boosted.fits <- c( 
		"hash12.neg3_10_5000.dat"
				)

exp <- 12

nHashes <- 2^exp

ngram_min <- 1
ngram_max <- 1

analyzer_type = 'english'


.jinit()
.jaddClassPath('lib/kaggle_stemming-all-1.0-SNAPSHOT.jar')

janalyzer <- .jnew('stemming.ES_Analyzer','xenglish')

.stem <- function(x){ if (!is.character(x)) x <- as.character(x); .jcall(janalyzer, "S", "stem", x)}
.tokenize <- function(x) str_split(str_trim(x), fixed(" "))

##
## prepare hashes
##
dfl <- .f.melt(df.in, mcols=cols)
colnames(dfl)[3] <- "answer"

dfl <- dfl %>% left_join(df.in %>% select(id, question), by="id")

dfl$text <- paste(dfl$question, dfl$answer)

tt <- lapply(dfl$text, function(x) NGramTokenizer(.stem(x), Weka_control(min = ngram_min, max = ngram_max)))

ch <- sapply(tt, function(x) paste(x, collapse = ','))

data <- data.frame(ngrams = ch, id = dfl$id, ida = dfl$ida)

m <- hashed.model.matrix( ~ split(ngrams, delim = ","), data, nHashes)

dd <- as.data.frame(as.matrix(m))

dfl <- dfl %>% select(id, ida) %>% bind_cols(dd)

colnames(dfl) <- make.names(colnames(dfl))

dfl.h <- dfl

for ( fitname in boosted.fits ) {
	filename.model <- paste("data/xgb.fit.", fitname, sep = '')
	
	filename.out <- gsub("dat$", "RData", paste(fdir, "df.quizlet.", fitname, sep=''))
	
	q.name <- gsub(".dat$", "", fitname)
		
	fit <- xgb.load(filename.model)
	
	m <- as.matrix(dfl.h %>% select(-c(id,ida))) 
	
	p <- predict(fit, newdata = m)
	
	dfl <- data.frame(id = dfl.h$id, ida = dfl.h$ida, boost = -p)
	colnames(dfl)[3] <- q.name
	
	if ( cols[1] == 4) print(calcMarginalAccuracy(df %>% left_join(dfl, by=c("id","ida"))))	
	
	save(dfl, file=filename.out)
}



