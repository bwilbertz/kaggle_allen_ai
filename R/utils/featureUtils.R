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
.nHits <- function(question, answer = NULL, index) {
	q <- if (is.null(answer)) question else paste("(", question, ") AND (", answer, ")", sep = '')
	
	return(.nHits.query(q, index))
}
			
.nHits.query <- function(query, index) index(index) %>% Search_(params = list(q = query)) %>% elasticdsl::n() 

.f.hits.abs <- function(df, index) .f.generic.abs(df, index, f = .nHits, name.prefix = "hits.abs")

.f.hits.rel <- function(df, index) .f.abs2rel(.f.hits.abs(df, index))

.f.generic.abs <- function(df, index, f, name.prefix, ...) {
	q0 <- sapply(.esc.str(df$question), function(x) f(x, index = index, ...))
	a1 <- sapply(1:nrow(df), function(i) f(.esc.str(df$question[i]), answer=.esc.str(df$answerA[i]), index = index, ...))
	a2 <- sapply(1:nrow(df), function(i) f(.esc.str(df$question[i]), answer=.esc.str(df$answerB[i]), index = index, ...))
	a3 <- sapply(1:nrow(df), function(i) f(.esc.str(df$question[i]), answer=.esc.str(df$answerC[i]), index = index, ...))
	a4 <- sapply(1:nrow(df), function(i) f(.esc.str(df$question[i]), answer=.esc.str(df$answerD[i]), index = index, ...))
	
	ans <- data.frame(id = df$id, q0 = q0, a1 = a1, a2 = a2, a3 = a3, a4 = a4)
	
	iname <- make.names(index)
	
	colnames(ans) <- c("id", paste(name.prefix, ".0_",iname, sep=''), 
			paste(name.prefix, ".1_",iname, sep=''),
			paste(name.prefix, ".2_",iname, sep=''),
			paste(name.prefix, ".3_",iname, sep=''),
			paste(name.prefix, ".4_",iname, sep=''))
	
	ans
}

.f.abs2rel <- function(df) {
	r1 <- df[,3] / df[,2]
	r2 <- df[,4] / df[,2]
	r3 <- df[,5] / df[,2]
	r4 <- df[,6] / df[,2]
	
	ans <- data.frame(df$id, r1, r2, r3, r4)
	
	colnames(ans) <- gsub("abs", "rel", colnames(df)[-2])
	
	ans
}

.f.melt <- function(df, mcols=2:5, id.vars="id", var.name = gsub("\\.1", "", colnames(df)[mcols[1]])) {
	require(reshape2)
	a.ids <- c("A","B","C","D")
	col.id <- which(colnames(df) == id.vars)
	
	cols <- c(col.id, mcols)
	
	ans <- melt(df[, cols], id.vars = id.vars)
	
	ida <- factor(paste(as.character(ans$id), ".", a.ids[as.numeric(ans$variable)], sep=''))
	
	ans$variable <- ida
	
	colnames(ans) <- c("id", "ida", var.name)
	
	ans
}

.esc.str <- function(str) gsub("([[:punct:]])", "\\\\\\1", str)

.remove.str <- function(str) tolower(gsub("[[:punct:]]", " ", str))



.query.simple <- function(question, answer = NULL, index, size = 10, default_operator = "OR", remove.wiki.internals = F, remove.wiki.special = F, answer.as.phrase = F, answer.slope = 0, debug = F, ...) {
	if (!is.null(answer) && answer.as.phrase) answer <- paste('"', answer, '"~', answer.slope, sep='') 
				
	q <- if (is.null(answer) || answer == "") question else paste("(", question, ") AND (", answer, ")", sep = '')
	
	if ( remove.wiki.internals ) q <- paste("(",q, ")", "AND NOT title:/Wikipedia:.*/")
	
	if ( remove.wiki.special ) q <- paste("(",q, ")", "AND special:false")
	
	if (debug) cat(q,"\n")
	
	return(index(index) %>% Search_(params = list(q = q, default_operator = default_operator), size = size))
}

.query.knn <- function(question, answer, all.answers, question.only = T, answer.only = T, answer.as.filter = F, index, k = 50, size = 10, default_operator = "OR", answer.as.phrase = F, answer.slope = 0, debug = F, ...) {
	if (answer.as.phrase) answer <- paste('"', answer, '"~', answer.slope, sep='') 
	
	q.all <- if (question.only ) question else paste("(", question, ") AND (", all.answers, ")", sep = '')
	
	q <- if (answer.only) answer else paste("(", question, ") AND (", answer, ")", sep = '')
	
	if ( answer.as.filter) {
		body <- list(query = list(filtered = list( 
								filter = list(
										query = list(query_string = list(query = all.answers, default_operator = default_operator))
								),
								query = list(query_string = list(query = question, default_operator = default_operator))
						)
				))
		docs <- Search(index = index, size = k, body = body)
	} else {
		docs <- index(index) %>% Search_(params = list(q = q.all, default_operator = default_operator), size = k)
	}
	
	nns <- lapply(docs$hits$hits, function(h) elastic::explain(index = h$`_index`, type=h$`_type`, id=h$`_id`, q = q, default_operator = default_operator))
	
	scores <- sapply(nns, function(x) x$explanation$value)
	
	ids <- order(scores, decreasing = T)
	
	if ( length(ids) > size) ids <- ids[1:size]
	
	return(nns[ids])
}


.query.knn.fast <- function(question, answer, all.answers, question.only = T, answer.only = T, answer.as.filter = F, index, k = 50, size = 10, default_operator = "OR", answer.as.phrase = F, answer.slope = 0, debug = F, ...) {
	if (answer.as.phrase) answer <- paste('"', answer, '"~', answer.slope, sep='') 
	
	q.all <- if (question.only ) question else paste("(", question, ") AND (", all.answers, ")", sep = '')
	
	q <- if (answer.only) answer else paste("(", question, ") AND (", answer, ")", sep = '')
	
	if ( answer.as.filter) {
		body <- list(query = list(filtered = list( 
								filter = list(
										query = list(query_string = list(query = all.answers, default_operator = default_operator))
								),
								query = list(query_string = list(query = question, default_operator = default_operator))
						)
				))
		docs <- Search(index = index, size = k, body = body)
	} else {
		docs <- index(index) %>% Search_(params = list(q = q.all, default_operator = default_operator), size = k)
	}
	
	knn.ids <- unlist(lapply(docs$hits$hits, function(h) h$`_id`))

	if ( length(knn.ids) == 0 ) return(docs)

	type <- docs$hits$hits[[1]]$`_type`
	
	body <- list(query = list(filtered = list( 
							filter = list(ids = list(type = type, values = knn.ids)),
							query = list(query_string = list(query = q, default_operator = default_operator))
					)
			)
	)
	
	if (debug) print(body)
	
	nns <- Search(index = index, size = size, body = body)
	
	nns
}


.explain.simple <- function(question, answer = NULL, index, size = 10, default_operator = "OR", remove.wiki.internals = F, remove.wiki.special = F, answer.as.phrase = F, answer.slope = 0, debug = F, ...) {
	if (!is.null(answer) && answer.as.phrase) answer <- paste('"', answer, '"~', answer.slope, sep='') 
	
	q <- if (is.null(answer) || answer == "") question else paste("(", question, ") AND (", answer, ")", sep = '')
	
	if ( remove.wiki.internals ) q <- paste("(",q, ")", "AND NOT title:/Wikipedia:.*/")
	
	if ( remove.wiki.special ) q <- paste("(",q, ")", "AND special:false")
	
	if (debug) cat(q,'\n')
	
	docs <- index(index) %>% Search_(params = list(q = q, default_operator = default_operator), size = size)
	
	lapply(docs$hits$hits, function(h) elastic::explain(index = h$`_index`, type=h$`_type`, id=h$`_id`, q = q, default_operator = default_operator, ...))
}

.query.conditional <- function(question, answer = NULL, index, size = 10, default_operator = "OR", remove.wiki.internals = F, remove.wiki.special = F, answer.as.phrase = F, answer.slope = 0, ...) {
	if (!is.null(answer)  && answer != "") {
		q.cond <- if (answer.as.phrase) paste('\\"', answer, '\\"\\~', answer.slope, sep='') else answer		
	}  
	
	q <- question
	
	if ( remove.wiki.internals ) q <- paste("(",q, ")", "AND NOT title:/Wikipedia:.*/")
	
	if ( remove.wiki.special ) q <- paste("(",q, ")", "AND special:false")
	
	if (is.null(answer) || answer == "") {
		
		
		body <- list(query = list(filtered = list( 
								filter = list(
										query = list(query_string = list(query = q, default_operator = default_operator))
								)
						)
				))
		
	} else {
		if ( remove.wiki.internals ) q.cond <- paste(q.cond, "AND NOT title:/Wikipedia:.*/")
		
		if ( remove.wiki.special ) q.cond <- paste("(",q.cond, ")", "AND special:false")
		
		body <- list(query = list(filtered = list( 
								filter = list(
										query = list(query_string = list(query = q, default_operator = default_operator))
								),
								query = list(query_string = list(query = q.cond, default_operator = default_operator))
						)
				))
		
	}
	
	return(Search(index = index, body = body, default_operator = default_operator, size = size))
}







.q.generic <- function(df, index, f, name.prefix, sanitizer = .esc.str, pureAnswers = F, ...) {
	q0 <- lapply(sanitizer(df$question), function(x) f(x, index = index, ...))
	a1 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerA[i]), index = index, ...))
	a2 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerB[i]), index = index, ...))
	a3 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerC[i]), index = index, ...))
	a4 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerD[i]), index = index, ...))
	
	iname <- make.names(index)
	
	if ( pureAnswers ) {	
		a1o <- lapply(1:nrow(df), function(i) f(sanitizer(df$answerA[i]), index = index, ...))
		a2o <- lapply(1:nrow(df), function(i) f(sanitizer(df$answerB[i]), index = index, ...))
		a3o <- lapply(1:nrow(df), function(i) f(sanitizer(df$answerC[i]), index = index, ...))
		a4o <- lapply(1:nrow(df), function(i) f(sanitizer(df$answerD[i]), index = index, ...))
		
		ans <- list(id = df$id, q0 = q0, a1 = a1, a2 = a2, a3 = a3, a4 = a4, a1o = a1o, a2o = a2o, a3o = a3o, a4o = a4o )
		
		names(ans) <- c("id", paste(name.prefix, ".0_",iname, sep=''), 
				paste(name.prefix, ".1_",iname, sep=''),
				paste(name.prefix, ".2_",iname, sep=''),
				paste(name.prefix, ".3_",iname, sep=''),
				paste(name.prefix, ".4_",iname, sep=''),
				paste(name.prefix, ".1o_",iname, sep=''),
				paste(name.prefix, ".2o_",iname, sep=''),
				paste(name.prefix, ".3o_",iname, sep=''),
				paste(name.prefix, ".4o_",iname, sep=''))
		
		return(ans)
	}
	
	
	ans <- list(id = df$id, q0 = q0, a1 = a1, a2 = a2, a3 = a3, a4 = a4)
	
	names(ans) <- c("id", paste(name.prefix, ".0_",iname, sep=''), 
			paste(name.prefix, ".1_",iname, sep=''),
			paste(name.prefix, ".2_",iname, sep=''),
			paste(name.prefix, ".3_",iname, sep=''),
			paste(name.prefix, ".4_",iname, sep='')
	)	
	
	ans
}

.knn.generic <- function(df, index, f, name.prefix, sanitizer = .esc.str, ...) {
	a1 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerA[i]), sanitizer(paste(df$answerA[i], df$answerB[i], df$answerC[i], df$answerD[i])), index = index, ...))
	a2 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerB[i]), sanitizer(paste(df$answerA[i], df$answerB[i], df$answerC[i], df$answerD[i])), index = index, ...))
	a3 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerC[i]), sanitizer(paste(df$answerA[i], df$answerB[i], df$answerC[i], df$answerD[i])), index = index, ...))
	a4 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), answer=sanitizer(df$answerD[i]), sanitizer(paste(df$answerA[i], df$answerB[i], df$answerC[i], df$answerD[i])), index = index, ...))
	
	iname <- make.names(index)
	
	ans <- list(id = df$id, a1 = a1, a2 = a2, a3 = a3, a4 = a4)
	
	names(ans) <- c("id",  
			paste(name.prefix, ".1_",iname, sep=''),
			paste(name.prefix, ".2_",iname, sep=''),
			paste(name.prefix, ".3_",iname, sep=''),
			paste(name.prefix, ".4_",iname, sep='')
	)	
	
	ans
}

.q.generic.inv <- function(df, index, f, name.prefix, sanitizer = .esc.str, ...) {
	q0 <- lapply(sanitizer(df$question), function(x) f(x, index = index, ...))
	a1 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), 
						answer=paste(sanitizer(df$answerB[i]),sanitizer(df$answerC[i]),sanitizer(df$answerD[i]), sep=' AND '), 
						index = index, ...))
	a2 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), 
						answer=paste(sanitizer(df$answerA[i]),sanitizer(df$answerC[i]),sanitizer(df$answerD[i]), sep=' AND '), 
						index = index, ...))
	a3 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), 
						answer=paste(sanitizer(df$answerA[i]),sanitizer(df$answerB[i]),sanitizer(df$answerD[i]), sep=' AND '), 
						index = index, ...))
	a4 <- lapply(1:nrow(df), function(i) f(sanitizer(df$question[i]), 
						answer=paste(sanitizer(df$answerA[i]),sanitizer(df$answerB[i]),sanitizer(df$answerC[i]), sep=' AND '), 
						index = index, ...))
	
	ans <- list(id = df$id, q0 = q0, a1 = a1, a2 = a2, a3 = a3, a4 = a4)
	
	iname <- make.names(index)
	
	names(ans) <- c("id", paste(name.prefix, ".0_",iname, sep=''), 
			paste(name.prefix, ".1_",iname, sep=''),
			paste(name.prefix, ".2_",iname, sep=''),
			paste(name.prefix, ".3_",iname, sep=''),
			paste(name.prefix, ".4_",iname, sep=''))
	
	ans
}

.q.explain.simple <- function(df, index, ...) .q.generic(df, index, f = .explain.simple, name.prefix="explain.simple", sanitizer = .remove.str, ...)

.q.common.terms <- function(df, index, cutoff = 0.001, ...) .q.generic(df, index, f = .query.common.terms, name.prefix=paste("common.terms.", cutoff, sep=''), sanitizer = .remove.str, cutoff.question = cutoff, cutoff.answer = cutoff, ...)

.q.simple.and.slope <- function(df, index, answer.slope = 10, ...) .q.generic(df, index, f = .query.simple, name.prefix=paste("simple.and.slope", answer.slope, sep=''), sanitizer = .remove.str, answer.slope, answer.as.phrase = T, ...)

.q.simple.and <- function(df, index, ...) .q.generic(df, index, f = .query.simple, name.prefix="simple.and", sanitizer = .remove.str, ...)

.q.glove.score <- function(df, index, ...) .q.generic(df, index, f = .query.glove.score, name.prefix=ifelse(use.idf, "glove.score.idf", "glove.score"), sanitizer = .remove.str, ...)

.q.knn.glove.score <- function(df, index, k,...) .knn.generic(df, index, f = .knn.glove.score, name.prefix=ifelse(use.idf, "knn.glove.score.idf", "knn.glove.score"), sanitizer = .remove.str, ...)

.q.knn <- function(df, index, k,...) .knn.generic(df, index, f = .query.knn, name.prefix=paste("knn.k",k, sep=''), sanitizer = .remove.str, ...)

.q.knn.fast <- function(df, index, k,...) .knn.generic(df, index, f = .query.knn.fast, name.prefix=paste("knn.k",k, sep=''), sanitizer = .remove.str, ...)

.q.all.and <- function(df, index, ...) .q.generic(df, index, f = .query.simple, name.prefix="all.and", default_operator = "AND", sanitizer = .remove.str, ...)

.q.cond.or <- function(df, index, ...) .q.generic(df, index, f = .query.conditional, name.prefix="cond.or", default_operator = "OR", sanitizer = .remove.str, ...)

.q.cond.and <- function(df, index, ...) .q.generic(df, index, f = .query.conditional, name.prefix="cond.and", default_operator = "AND", sanitizer = .remove.str, ...)


.sum.top.scores <- function(x, top_n = 10) {
	if(x$hits$total == 0) return(0)
	
	hits <- x$hits$hits
	
	n <- min(top_n, length(hits))
	
	sum(sapply(1:n, function(i) hits[[i]]$`_score`))
}

.explain.sum.top.score.perWord.answer <- function(x, nWords, top_n = 10) {
	if (length(x) == 0) return(0)
	
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) x[[i]]$explanation$details[[2]]$value / nWords))
}

.explain.sum.top.score.perWord.question <- function(x, nWords, top_n = 10) {
	if (length(x) == 0) return(0)
	
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) x[[i]]$explanation$details[[1]]$value / nWords))
}

.explain.sum.top.match.ratio.answer <- function(x, nWords, top_n = 10) {
	if (length(x) == 0) return(0)
	
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) length(x[[i]]$explanation$details[[2]]$details) / nWords))
}

.explain.sum.top.match.ratio.question <- function(x, nWords, top_n = 10) {
	if (length(x) == 0) return(0)
	
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) length(x[[i]]$explanation$details[[1]]$details) / nWords))
}

.explain.sum.top.scores.answer <- function(x, top_n = 10) {
	if (length(x) == 0) return(0)
			
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) x[[i]]$explanation$details[[2]]$value))
}

.explain.sum.top.scores.question <- function(x, top_n = 10) {
	if (length(x) == 0) return(0)
	
	n <- min(top_n, length(x))
	
	sum(sapply(1:n, function(i) x[[i]]$explanation$details[[1]]$value))
}


calcRanks <- function(df, rk.prefix = "rk.", exclude.pattern = c("^rk\\.","^d2b\\.", "_question_")) {
	cc <- which(sapply(df[1,], is.numeric))
	
	for ( pattern in exclude.pattern ) cc <- cc[which(!grepl(pattern,names(cc)))]
	
	ans <- df %>% select(id, ida)
	
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		cat(col, names(col), "\n\n")
		ncol <- paste("`",names(col),"`", sep='')
		
	
		df.rank <- df %>% select_("id", "ida", ncol) %>%
				group_by(id) %>%
				arrange_(paste("desc(", ncol,")", sep=''))  %>%
				mutate(rank=row_number())
		
		
		colnames(df.rank)[4] <- paste(rk.prefix, names(col), sep='')
		
		df.rank <- df.rank %>% select(-3)
		ans <- ans %>% left_join(df.rank, by=c("id", "ida"))
	}
	
	ans %>% select(-id)
}


calcDist2Best <- function(df, d2b.prefix = "d2b.", exclude.pattern = c("^rk\\.","^d2b\\.", "_question_")) {
	cc <- which(sapply(df[1,], is.numeric))
	
	for ( pattern in exclude.pattern ) cc <- cc[which(!grepl(pattern,names(cc)))]
	
	ans <- df %>% select(id, ida)
	
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		cat(col, names(col), "\n\n")
		ncol <- paste("`",names(col),"`", sep='')
		
		df.one <- df %>% select_("id", "ida", ncol)
		
		df.d2b <- df.one %>% group_by(id) %>%
				arrange_(paste("desc(", ncol,")", sep=''))  %>%
				summarise_(max = paste("max(", ncol,")", sep=''))
		
		df.d2b <- df.one %>% left_join(df.d2b, by="id") %>% 
				mutate_(d2b = paste("max-", ncol, sep='')) %>% select(id, ida, d2b)
		
		
		colnames(df.d2b)[3] <- paste(d2b.prefix, names(col), sep='')

		ans <- ans %>% left_join(df.d2b, by=c("id", "ida"))
	}
	
	ans %>% select(-id)
}

calcDist2Mean <- function(df, d2b.prefix = "d2m.", exclude.pattern = c("^rk\\.","^d2b\\.", "_question_")) {
	cc <- which(sapply(df[1,], is.numeric))
	
	for ( pattern in exclude.pattern ) cc <- cc[which(!grepl(pattern,names(cc)))]
	
	ans <- df %>% select(id, ida)
	
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		cat(col, names(col), "\n\n")
		ncol <- paste("`",names(col),"`", sep='')
		
		df.one <- df %>% select_("id", "ida", ncol)
		
		df.d2b <- df.one %>% group_by(id) %>%
				arrange_(paste("desc(", ncol,")", sep=''))  %>%
				summarise_(m = paste("mean(", ncol,")", sep=''))
		
		df.d2b <- df.one %>% left_join(df.d2b, by="id") %>% 
				mutate_(d2b = paste("m-", ncol, sep='')) %>% select(id, ida, d2b)
		
		
		colnames(df.d2b)[3] <- paste(d2b.prefix, names(col), sep='')
		
		ans <- ans %>% left_join(df.d2b, by=c("id", "ida"))
	}
	
	ans %>% select(-id)
}

calcDist2StdMean <- function(df, d2b.prefix = "d2sm.", exclude.pattern = c("^rk\\.","^d2b\\.", "_question_")) {
	cc <- which(sapply(df[1,], is.numeric))
	
	for ( pattern in exclude.pattern ) cc <- cc[which(!grepl(pattern,names(cc)))]
	
	ans <- df %>% select(id, ida)
	
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		cat(col, names(col), "\n\n")
		ncol <- paste("`",names(col),"`", sep='')
		
		df.one <- df %>% select_("id", "ida", ncol)
		
		df.d2b <- df.one %>% group_by(id) %>%
				arrange_(paste("desc(", ncol,")", sep=''))  %>%
				summarise_(m = paste("mean(", ncol,")", sep=''), max=paste("max(", ncol,")", sep=''),min = paste("min(", ncol,")", sep=''))

		df.d2b$range <- df.d2b$max - df.d2b$min
		df.d2b$range <- ifelse(df.d2b$range == 0, 1, df.d2b$range)
		
		df.d2b <- df.one %>% left_join(df.d2b, by="id") %>% 
				mutate_(d2b = paste("(", ncol, "-m)/range", sep='')) %>% select(id, ida, d2b)
		
		
		colnames(df.d2b)[3] <- paste(d2b.prefix, names(col), sep='')
		
		ans <- ans %>% left_join(df.d2b, by=c("id", "ida"))
	}
	
	ans %>% select(-id)
}


calcDist2Last <- function(df, d2b.prefix = "d2l.", exclude.pattern = c("^rk\\.","^d2b\\.","^d2m\\.", "_question_")) {
	cc <- which(sapply(df[1,], is.numeric))
	
	for ( pattern in exclude.pattern ) cc <- cc[which(!grepl(pattern,names(cc)))]
	
	ans <- df %>% select(id, ida)
	
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		cat(col, names(col), "\n\n")
		ncol <- paste("`",names(col),"`", sep='')
		
		df.one <- df %>% select_("id", "ida", ncol)
		
		df.d2b <- df.one %>% group_by(id) %>%
				arrange_(paste("desc(", ncol,")", sep=''))  %>%
				summarise_(m = paste("min(", ncol,")", sep=''))
		
		df.d2b <- df.one %>% left_join(df.d2b, by="id") %>% 
				mutate_(d2b = paste("m-", ncol, sep='')) %>% select(id, ida, d2b)
		
		
		colnames(df.d2b)[3] <- paste(d2b.prefix, names(col), sep='')
		
		ans <- ans %>% left_join(df.d2b, by=c("id", "ida"))
	}
	
	ans %>% select(-id)
}

calcMarginalAccuracy <- function(df) {
	cc <- which(sapply(df[1,], is.numeric))
	
	df.q <- df %>% group_by(id) 
	
	fnames <- list()
	accs <- c()
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		x <- df.q %>% arrange_(paste("desc(", names(col), ")", sep="`")) %>% summarise(pred = first(Class))
		
		acc <- sum(as.logical(as.character(x$pred))) / nrow(x)
		
		accs <- c(accs, acc)
		fnames[[i]] <- colnames(df)[col]
	}
	
	ord <- order(accs, decreasing = T)
	
	ans <- data.frame(accuracy = accs[ord])
	row.names(ans) <- fnames[ord]
	
	ans
}

calcMarginalPredictions <- function(df) {
	cc <- which(sapply(df[1,], is.numeric))
	
	df.q <- df %>% group_by(id) 
	
	ans <- data.frame(id = levels(df$id))
	for ( i in 1:length(cc) ) {
		col <- cc[i]
		x <- df.q %>% arrange_(paste("desc(", names(col), ")", sep="`")) %>% summarise(pred = first(Class))
		x$pred <- as.logical(as.character(x$pred))
		colnames(x)[2] <- names(col)
		
		ans <- ans %>% left_join(x, by = "id")				
	}
	
	ans
}



