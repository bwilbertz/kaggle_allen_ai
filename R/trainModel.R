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

dir <- "features/train/"
data.dir <- "data/"
train.id <- "retrain_20160130"

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



df$Class <- factor(ifelse(df$Class, "T", "F"), levels = c("T","F"))


# load all feature files and merge into df
for ( f in feature.files) {
        cat(f,"\n")
        load(paste(dir, f, sep=''))

        df <- df %>% left_join(dfl, by = c("id", "ida"))
}

##
##
load(paste(dir, "df.basic.stats.RData", sep=''))
df$top10.slope10.max2.q <- ifelse(dfl.words$maxWords <=2, df$top10Score.abs.simple.and.slope10_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25, df$top10Score.abs.simple.and_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25)
df$top10.knn.k50.slope10.max2.q <- ifelse(dfl.words$maxWords <=2, df$top10Score.knn.k50.slope10_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25, df$top10Score.knn.k50_simplewiki.english.bm25.wiki2json.byparagraph.enwiki.english.bm25.wiki2json.byparagraph.enwikibooks.english.bm25.wiki2json.byparagraph.ck12.english.bm25.ck12_lessons.english.bm25.books.english.bm25.quizlet.english.bm25)
df$top10.slope10.max2.q2 <- ifelse(dfl.words$maxWords <=2, df$top10Score.abs.simple.and.slope10_quizlet_20160126.english.bm25, df$top10Score.abs.simple.and_quizlet_20160126.english.bm25)


df <- df %>% select(-contains("_question_"))

df <- df %>% select(-contains(".aword."))


mp <- calcMarginalAccuracy(df)
mp

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


##
## transform for ranking
##
df.c <- df %>% filter(Class == "T")
df.w1 <- df %>% filter(Class == "F") %>% group_by(id) %>% filter(row_number()==1)
df.w2 <- df %>% filter(Class == "F") %>% group_by(id) %>% filter(row_number()==2)
df.w3 <- df %>% filter(Class == "F") %>% group_by(id) %>% filter(row_number()==3)

df.1 <- bind_rows(df.c, df.w1) %>% arrange(id)
df.2 <- bind_rows(df.c, df.w2) %>% arrange(id)
df.3 <- bind_rows(df.c, df.w3) %>% arrange(id)

df.xrk <- bind_rows(df.1, df.2, df.3)


##
## training
##
df.orig <- df
df <- df.xrk

y <- as.numeric(df$Class) - 1
x <- df %>% select(-c(id, ida, Class))


nBootStrapIters <- 25

q.ids <- factor(levels(df$id))
df.i <- df %>% select(id,ida)
df.i$idx <- 1:nrow(df.i)

index.q <- createResample(q.ids, nBootStrapIters)
indexOut.q <- lapply(index.q, function(i) (1:length(q.ids))[-i])

index <- lapply(index.q, function(i) { df.q <- data_frame(id = q.ids[i]) %>%
                                        left_join(df.i, by="id"); df.q$idx})

indexOut <- lapply(indexOut.q, function(i) {  df.q <- data_frame(id = q.ids[i]) %>%
                                        left_join(df.i, by="id") %>% distinct(ida); df.q$idx})


## set caret options
stats <-  function(...) c(qAccuracyRank(..., ids = df$id), defaultSummary(...))

ctrl <- trainControl(method = "boot",
                number = nBootStrapIters,
                index = index,
                indexOut = indexOut,
                summaryFunction = stats,
                verboseIter = T)


##
## and fit
##
model <- caret.xgb.rank2()
model$grid <- function (x, y, len = NULL, search = "grid") {
       if (search == "grid") {
               out <- expand.grid(max_depth = seq(1, len), nrounds = floor((1:len) * 
                                                               150), eta = c(0.3), gamma = 0, colsample_bytree = c(0.6),
                                               min_child_weight = c(1))
       }
       else {
               out <- data.frame(nrounds = sample(1:1000, size = len * 
                                                               10, replace = TRUE), max_depth = sample(1:10, replace = TRUE, 
                                               size = len), eta = runif(len, min = 0.001, max = 0.6), 
                               gamma = runif(len * 10, min = 0, max = 10), colsample_bytree = runif(len * 
                                                               5, min = 0.3, max = 0.7), min_child_weight = sample(0:20, 
                                               size = len * 5, replace = TRUE))
               out$nrounds <- floor(out$nrounds)
               out <- out[!duplicated(out), ]
       }
       out
}

fit <- train(x,y, method=model, trControl = ctrl, metric = "qAccuracy", tuneLength=1)
fit

imp <- varImp(fit)
imp

save(fit, file=paste(data.dir, "caret.fit.", train.id, ".RData", sep=''))
xgb.save(fit$finalModel, paste(data.dir, "xgb.fit.", train.id, ".dat", sep=''))


