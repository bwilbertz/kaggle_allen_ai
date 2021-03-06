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

# we have to fix some data issue in the files
sanitize.str <- function(str) gsub("([a-z])\\.([A-Za-z][a-z])", "\\1 \\2", str)

file.train <- "input/training_set.tsv"
file.test.final <- "input/test_set.tsv"
file.out <- "input/data.RData"

# train
df.train <- read.csv(file.train, header = T, sep = "\t", stringsAsFactors = F)

df.train$question <- sanitize.str(df.train$question)
df.train$answerA <- sanitize.str(df.train$answerA)
df.train$answerB <- sanitize.str(df.train$answerB)
df.train$answerC <- sanitize.str(df.train$answerC)
df.train$answerD <- sanitize.str(df.train$answerD)

df.train$id <- factor(df.train$id)
df.train$correctAnswer <- factor(df.train$correctAnswer)


# test
df.test.final <- read.csv(file.test.final, header = T, sep = "\t", stringsAsFactors = F)

df.test.final$question <- sanitize.str(df.test.final$question)
df.test.final$answerA <- sanitize.str(df.test.final$answerA)
df.test.final$answerB <- sanitize.str(df.test.final$answerB)
df.test.final$answerC <- sanitize.str(df.test.final$answerC)
df.test.final$answerD <- sanitize.str(df.test.final$answerD)

df.test.final$id <- factor(df.test.final$id)


##
## create binary classification problem of training data
##
library(reshape2)
library(dplyr)

correct <- paste(as.character(df.train$id), as.character(df.train$correctAnswer), sep=".")

ida <- factor(c(paste(df.train$id, "A", sep="."), 
				paste(df.train$id, "B", sep="."), 
				paste(df.train$id, "C", sep="."), 
				paste(df.train$id, "D", sep=".")))

df <- data.frame(id = rep(df.train$id, 4), ida = ida)

df.c <- data.frame(ida = factor(correct, levels = levels(df$ida)), Class = rep(T, length(correct)))

df <- df %>% left_join(df.c, by="ida")

df$Class[which(is.na(df$Class))] <- F

table(df$Class) / nrow(df)


save(df, df.train, df.test.final, file = file.out)

