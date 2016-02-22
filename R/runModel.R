# TODO: Add comment
# 
# Author: benedikt
###############################################################################




library(plyr)
library(dplyr)
library(caret)
library(xgboost)

source("utils/featureUtils.R", echo=FALSE, encoding="UTF-8")
source("utils/predictionUtils.R", echo=FALSE, encoding="UTF-8")
source("utils/submissionUtils.R", echo=FALSE, encoding="UTF-8")

load("input/data.RData")

train.id <- "20160130"

fit.xgb <- xgb.load("data/fit_20160130_rank_lpmi2.dat")

dir <- "features/test_final/"

sub.dir <- "output/"

feature.files <- c(
		"df.hits.simple.And.quizlet.RData",
		"df.maxScore.simple.And.quizlet.RData",
		"df.top3Score.simple.And.quizlet.RData",
		"df.top5Score.simple.And.quizlet.RData",
		"df.top10Score.simple.And.quizlet.RData",
		"df.top50Score.simple.And.quizlet.RData",
		"df.hits.simple.And.quizlet_20160126.RData",
		"df.maxScore.simple.And.quizlet_20160126.RData",
		"df.top3Score.simple.And.quizlet_20160126.RData",
		"df.top5Score.simple.And.quizlet_20160126.RData",
		"df.top10Score.simple.And.quizlet_20160126.RData",
		"df.top50Score.simple.And.quizlet_20160126.RData",
		"df.hits.simple.And.quizlet.slope10_20160126.RData",
		"df.maxScore.simple.And.quizlet.slope10_20160126.RData",
		"df.top3Score.simple.And.quizlet.slope10_20160126.RData",
		"df.top5Score.simple.And.quizlet.slope10_20160126.RData",
		"df.top10Score.simple.And.quizlet.slope10_20160126.RData",
		"df.top50Score.simple.And.quizlet.slope10_20160126.RData",
		"df.maxScore.simple.And.seeccbq.RData",
		"df.top3Score.simple.And.seeccbq.RData",
		"df.top5Score.simple.And.seeccbq.RData",
		"df.top10Score.simple.And.seeccbq.RData",
		"df.top50Score.simple.And.seeccbq.RData",
		"df.hits.simple.And.seeccbq.slope10.RData",
		"df.maxScore.simple.And.seeccbq.slope10.RData",
		"df.top3Score.simple.And.seeccbq.slope10.RData",
		"df.top5Score.simple.And.seeccbq.slope10.RData",
		"df.top10Score.simple.And.seeccbq.slope10.RData",
		"df.top50Score.simple.And.seeccbq.slope10.RData",
		"df.maxScore.knn.k50.seeccbq.RData",
		"df.top3Score.knn.k50.seeccbq.RData",
		"df.top5Score.knn.k50.seeccbq.RData",
		"df.top10Score.knn.k50.seeccbq.RData",
		"df.top50Score.knn.k50.seeccbq.RData",
		"df.maxScore.knn.k50.seeccbq.slope10.RData",
		"df.top3Score.knn.k50.seeccbq.slope10.RData",
		"df.top5Score.knn.k50.seeccbq.slope10.RData",
		"df.top10Score.knn.k50.seeccbq.slope10.RData",
		"df.top50Score.knn.k50.seeccbq.slope10.RData",
		"df.maxScore.explain.simple.seeccbq.RData",
		"df.top3Score.explain.simple.seeccbq.RData",
		"df.top5Score.explain.simple.seeccbq.RData",
		"df.top10Score.explain.simple.seeccbq.RData",
		"df.maxScore.matchRatio.explain.simple.seeccbq.RData",
		"df.top3Score.matchRatio.explain.simple.seeccbq.RData",
		"df.top5Score.matchRatio.explain.simple.seeccbq.RData",
		"df.top10Score.matchRatio.explain.simple.seeccbq.RData",
		"df.maxScore.perWord.explain.simple.seeccbq.RData",
		"df.top3Score.perWord.explain.simple.seeccbq.RData",
		"df.top5Score.perWord.explain.simple.seeccbq.RData",
		"df.top10Score.perWord.explain.simple.seeccbq.RData",
		"df.maxScore.explain.simple.seeccbq.slope10.RData",
		"df.top3Score.explain.simple.seeccbq.slope10.RData",
		"df.top5Score.explain.simple.seeccbq.slope10.RData",
		"df.top10Score.explain.simple.seeccbq.slope10.RData",
		"df.maxScore.matchRatio.explain.simple.seeccbq.slope10.RData",
		"df.top3Score.matchRatio.explain.simple.seeccbq.slope10.RData",
		"df.top5Score.matchRatio.explain.simple.seeccbq.slope10.RData",
		"df.top10Score.matchRatio.explain.simple.seeccbq.slope10.RData",
		"df.maxScore.perWord.explain.simple.seeccbq.slope10.RData",
		"df.top3Score.perWord.explain.simple.seeccbq.slope10.RData",
		"df.top5Score.perWord.explain.simple.seeccbq.slope10.RData",
		"df.top10Score.perWord.explain.simple.seeccbq.slope10.RData",
		"df.lpmi.mean.secbq2.tcm_50_xenglish.RData",
		"df.lpmi.median.secbq2.tcm_50_xenglish.RData",
		"df.lpmi.q25.secbq2.tcm_50_xenglish.RData",
		"df.quizlet.hash12.neg3_10_5000.RData",
		"df.coProb.mean.bm25.en-si-ck.NVAO_glove_50_500.RData"
)


ida <- factor(c(paste(df.test.final$id, "A", sep="."), 
				paste(df.test.final$id, "B", sep="."), 
				paste(df.test.final$id, "C", sep="."), 
				paste(df.test.final$id, "D", sep=".")))

df <- data.frame(id = rep(df.test.final$id, 4), ida = ida)

# load all feature files and merge into df
for ( f in feature.files) {
	cat(f,"\n")
	load(paste(dir, f, sep=''))
	
	df <- df %>% left_join(dfl, by = c("id", "ida"))
}


##
## handcrafted features
##
load(paste(dir, "df.basic.stats.RData", sep=''))
df$top10.slope10.max2.q <- ifelse(dfl.words$maxWords <=2, df$top10Score.abs.simple.and.slope10_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25, df$top10Score.abs.simple.and_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25)
df$top10.knn.k50.slope10.max2.q <- ifelse(dfl.words$maxWords <=2, df$top10Score.knn.k50.slope10_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25, df$top10Score.knn.k50_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25)
df$top10.slope10.max2.q2 <- ifelse(dfl.words$maxWords <=2, df$top10Score.abs.simple.and.slope10_quizlet_20160126.english.bm25, df$top10Score.abs.simple.and_quizlet_20160126.english.bm25)


df <- df %>% select(-contains("_question_"))

df <- df %>% select(-contains(".aword."))

load(paste(dir, "df.basic.stats.RData", sep=''))
df <- df %>% left_join(dfl.words, by = c("id", "ida"))

## add ranks and dist2best
df.rank <- calcRanks(df)

df.d2b <- calcDist2Best(df)

df.d2m <- calcDist2Mean(df)

df.d2sm <- calcDist2StdMean(df)

df <- df %>% left_join(df.rank, by ="ida") %>% left_join(df.d2b, by="ida") %>% left_join(df.d2m, by="ida") %>% left_join(df.d2sm, by="ida")

## add basic stats
load(paste(dir, "df.basic.stats.RData", sep=''))

df <- df %>% left_join(dfl.words, by = c("id", "ida")) %>% left_join(dfl.sentences, by = c("id", "ida"))



## create prediction sets
x <- df %>% select(-c(id, ida))


#pr <- predict(fit,x)
pr <- predict(fit.xgb, as.matrix(x))

p <- data.frame(T = -pr)

df.submission <- deriveAnswers(p, df$id, df$ida)

head(df.submission)


##
## write out
##
file <- paste(sub.dir, "sub_", train.id, ".csv", sep='')


write.table(df.submission, file = file, quote = F, row.names = F, sep=",")


