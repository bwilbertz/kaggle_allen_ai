# TODO: Add comment
# 
# Author: benedikt
###############################################################################

library(jsonlite)
library(stringr)
library(dplyr)

dir <- "tmp/quizlet/"

filename.out <- "tmp/df.quizlet.RData"
filename.json <- "tmp/quizlet.json"
		
files <- list.files(dir,"*.json", full.names = T)

l <- lapply(files, function(f) {fid <- file(f); js <- fromJSON(readLines(fid)); close(fid); if ( length(js$terms) > 0 && js$lang_terms == "en" && js$lang_definitions == "en") js$terms %>% select(-image)})
#l <- lapply(1:length(files), function(i) {fid <- file(files[i]); js <- fromJSON(readLines(fid)); close(fid); if ( js$lang_terms == "en" && js$lang_definitions == "en") js$terms %>% select(-image)})

df <- bind_rows(l)

ids.0 <- which(!nzchar(df$term) | !nzchar(df$definition))

length(ids.0)
df <- df[-ids.0,]

save(df, file=filename.out)

# create pairs
pairs <- str_replace_all(str_replace_all(paste(df$definition, df$term), "[\r\n]" , " "), "_", "")


##
## directly into json
## 
dfj <- data.frame(title=df$id, text=pairs)

fid <- file(filename.json)
stream_out(dfj, fid, pagesize=500000)


dir <- "tmp/quizlet_20160126/"

filename.out <- "tmp/df.quizlet_20160126.RData"
filename.json <- "tmp/quizlet_20160126.json"

files <- list.files(dir,"*.json", full.names = T)

l <- lapply(files, function(f) {fid <- file(f); js <- fromJSON(readLines(fid)); close(fid); if ( length(js$terms) > 0 && js$lang_terms == "en" && js$lang_definitions == "en") js$terms %>% select(-image)})
#l <- lapply(1:length(files), function(i) {fid <- file(files[i]); js <- fromJSON(readLines(fid)); close(fid); if ( js$lang_terms == "en" && js$lang_definitions == "en") js$terms %>% select(-image)})

df <- bind_rows(l)

ids.0 <- which(!nzchar(df$term) | !nzchar(df$definition))

length(ids.0)
df <- df[-ids.0,]

save(df, file=filename.out)

# create pairs
pairs <- str_replace_all(str_replace_all(paste(df$definition, df$term), "[\r\n]" , " "), "_", "")


##
## directly into json
## 
dfj <- data.frame(title=df$id, text=pairs)

fid <- file(filename.json)
stream_out(dfj, fid, pagesize=500000)


