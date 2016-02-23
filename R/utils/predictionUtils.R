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


qAccuracy <- function(data, lev = NULL, model = NULL, ids) {
	require(dplyr)
	ids.sample <- ids[data$rowIndex]
	
	data$ids <- ids.sample
	
	df.q <- data %>% group_by(ids) %>% arrange(desc(T)) %>% summarise(pred = first(obs))
	
	df.q$obs <- factor(rep("T", nrow(df.q)), levels = lev)
	
	df.q <- as.data.frame(df.q %>% select(-ids))
	
	ans <- defaultSummary(df.q, lev, model)
	
	names(ans) <- c("qAccuracy", "qKappa")
	
	ans
}


qAccuracyRank <- function(data, lev = NULL, model = NULL, ids) {	
	require(dplyr)
	ids.sample <- ids[data$rowIndex]
	
	data$ids <- ids.sample
	
	df.q <- data %>% group_by(ids) %>% arrange(pred) %>% summarise(pred = first(obs))
	
	lev <- c("T","F")
	
	df.q$obs <- factor(rep("T", nrow(df.q)), levels = lev)
	df.q$pred <- factor(ifelse(df.q$pred == 0, "T", "F"), levels = lev)
	
	df.q <- as.data.frame(df.q %>% select(-ids))
	
	ans <- defaultSummary(df.q, lev, model)
	
	names(ans) <- c("qAccuracy", "qKappa")
	
	ans
}


caret.xgb.rank.pairwise <- function() {
	m <- list(type =  c("Regression"),
			library = c("xgboost", "plyr"),
			label = "eXtreme Gradient Boosting rank:pairwise",
			tags = c("gradient boosting", "Learn2Rank"),
			loop = function (grid) {
				loop <- ddply(grid, c("eta", "max_depth"), function(x) c(nrounds = max(x$nrounds)))
				submodels <- vector(mode = "list", length = nrow(loop))
				for (i in seq(along = loop$nrounds)) {
					index <- which(grid$max_depth == loop$max_depth[i] & 
									grid$eta == loop$eta[i])
					trees <- grid[index, "nrounds"]
					submodels[[i]] <- data.frame(nrounds = trees[trees != 
											loop$nrounds[i]])
				}
				list(loop = loop, submodels = submodels)
			})
	
	m$parameters <- data.frame(parameter = c("nrounds","max_depth", "eta"),
			class = rep("numeric", 3),
			label = c("# Boosting Iterations","Max Tree Depth","Shrinkage"))
	
	m$grid <- function (x, y, len = NULL, search = "grid") {
		if (search == "grid") {
			out <- expand.grid(max_depth = seq(1, len), nrounds = floor((1:len) * 
									50), eta = 0.3)
		}
		else {
			out <- data.frame(nrounds = sample(1:1000, size = len * 
									10, replace = TRUE), max_depth = sample(1:10, replace = TRUE, 
							size = len), eta = runif(len, min = 0.001, max = 0.6))
			out$nrounds <- floor(out$nrounds)
			out <- out[!duplicated(out), ]
		}
		out
	}
	
	m$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
		dat <- xgb.DMatrix(as.matrix(x), label = y, group = rep(2, length(y)/2) )
		out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth), 
				data = dat, nrounds = param$nrounds, objective = "rank:pairwise", 
				...)
		
		out
	}
	
	m$predict <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$prob <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (length(modelFit$obsLevels) == 2) {
			out <- cbind(out, 1 - out)
			colnames(out) <- modelFit$obsLevels
		}
		else {
			out <- matrix(out, ncol = length(modelFit$obsLevels), 
					byrow = TRUE)
			colnames(out) <- modelFit$obsLevels
		}
		out <- as.data.frame(out)
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (length(modelFit$obsLevels) == 2) {
					tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				else {
					tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
							byrow = TRUE)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				tmp_pred <- as.data.frame(tmp_pred)
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$predictor <- function (x, ...) {
		imp <- xgb.importance(x$xNames, model = x)
		x$xNames[x$xNames %in% imp$Feature]
	}
	
	m$varImp <- function (object, numTrees = NULL, ...) {
		imp <- xgb.importance(object$xNames, model = object)
		imp <- as.data.frame(imp)[, 1:2]
		rownames(imp) <- as.character(imp[, 1])
		imp <- imp[, 2, drop = FALSE]
		colnames(imp) <- "Overall"
		imp
	}
	
	m$levels <- function (x) x$obsLevels 
	
	m$sort <- function (x) {
		x[order(x$nrounds, x$max_depth, x$eta), ]
	}
	
	m
}


caret.xgb.linear <- function() {
	m <- list(type =  c("Regression","Classification"),
			library = c("xgboost"),
			label = "eXtreme Gradient Boosting linear booster",
			tags = c("Linear Classifier Models", "Linear Regression Models",
					"L1 Regularization Models", "L2 Regularization Models",  
					"Boosting", "Ensemble Model", "Implicit Feature Selection"),
			loop = NULL)
	
	m$parameters <- data.frame(parameter = c("nrounds","lambda", "alpha"),
			class = rep("numeric", 3),
			label = c("# Boosting Iterations","L2 Regularization","L2 Regularization"))
	
	m$grid <- function (x, y, len = NULL, search = "grid") {
		if (search == "grid") {
			out <- expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 
											1)), alpha = c(0, 10^seq(-1, -4, length = len - 1)), 
					nrounds = floor((1:len) * 50))
		}
		else {
			out <- data.frame(lambda = 10^runif(len, min = -5, 0), 
					alpha = 10^runif(len, min = -5, 0), nrounds = sample(1:100, 
							size = len * 10, replace = TRUE))
		}
		out
	}
	
	m$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
		if (is.factor(y)) {
			if (length(lev) == 2) {
				y <- ifelse(y == lev[1], 1, 0)
				dat <- xgb.DMatrix(as.matrix(x), label = y)
				out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
						data = dat, nrounds = param$nrounds, objective = "binary:logistic", 
						booster = "gblinear", ...)
			}
			else {
				y <- as.numeric(y) - 1
				dat <- xgb.DMatrix(as.matrix(x), label = y)
				out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
						data = dat, num_class = length(lev), nrounds = param$nrounds, 
						objective = "multi:softprob", booster = "gblinear", ...)
			}
		}
		else {
			dat <- xgb.DMatrix(as.matrix(x), label = y)
			out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
					data = dat, nrounds = param$nrounds, objective = "reg:linear", 
					booster = "gblinear",...)
		}
		out
	}	
	
	m$predict <- function (modelFit, newdata, submodels = NULL) 	{
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (modelFit$problemType == "Classification") {
			if (length(modelFit$obsLevels) == 2) {
				out <- ifelse(out >= 0.5, modelFit$obsLevels[1], 
						modelFit$obsLevels[2])
			}
			else {
				out <- matrix(out, ncol = length(modelFit$obsLevels), 
						byrow = TRUE)
				out <- modelFit$obsLevels[apply(out, 1, which.max)]
			}
		}
		out
	}

	m$prob <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (length(modelFit$obsLevels) == 2) {
			out <- cbind(out, 1 - out)
			colnames(out) <- modelFit$obsLevels
		}
		else {
			out <- matrix(out, ncol = length(modelFit$obsLevels), 
					byrow = TRUE)
			colnames(out) <- modelFit$obsLevels
		}
		as.data.frame(out)
	}
	
	
	m$predictors <- function (x, ...) {
		imp <- xgb.importance(x$xNames, model = x)
		x$xNames[x$xNames %in% imp$Feature]
	}
	
	m$varImp <- function (object, numTrees = NULL, ...) {
		imp <- xgb.importance(object$xNames, model = object)
		imp <- as.data.frame(imp)[, 1:2]
		rownames(imp) <- as.character(imp[, 1])
		imp <- imp[, 2, drop = FALSE]
		colnames(imp) <- "Overall"
		imp
	}
	
	m$levels <- function (x) x$obsLevels 
	
	m$sort <- function (x) {
		x[order(x$nrounds, x$alpha, x$lambda), ]
	}
	
	m
}


caret.xgb.rank.linear <- function() {
	m <- list(type =  c("Regression"),
			library = c("xgboost", "plyr"),
			label = "eXtreme Gradient Boosting linear booster rank:pairwise",
			tags = c("gradient boosting", "Learn2Rank"),
			loop = NULL
	)
	
	m$parameters <- data.frame(parameter = c("nrounds","lambda", "alpha"),
			class = rep("numeric", 3),
			label = c("# Boosting Iterations","L2 Regularization","L2 Regularization"))
	
	m$grid <- function (x, y, len = NULL, search = "grid") {
		if (search == "grid") {
			out <- expand.grid(lambda = c(0, 10^seq(-1, -4, length = len - 
											1)), alpha = c(0, 10^seq(-1, -4, length = len - 1)), 
					nrounds = floor((1:len) * 50))
		}
		else {
			out <- data.frame(lambda = 10^runif(len, min = -5, 0), 
					alpha = 10^runif(len, min = -5, 0), nrounds = sample(1:100, 
							size = len * 10, replace = TRUE))
		}
		out
	}
	
	m$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
		dat <- xgb.DMatrix(as.matrix(x), label = y, group = rep(2, length(y)/2) )
		out <- xgb.train(list(lambda = param$lambda, alpha = param$alpha), 
				data = dat, nrounds = param$nrounds, booster = "gblinear", 
				objective = "rank:pairwise", ...)
		
		out
	}
	
	m$predict <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$prob <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (length(modelFit$obsLevels) == 2) {
			out <- cbind(out, 1 - out)
			colnames(out) <- modelFit$obsLevels
		}
		else {
			out <- matrix(out, ncol = length(modelFit$obsLevels), 
					byrow = TRUE)
			colnames(out) <- modelFit$obsLevels
		}
		out <- as.data.frame(out)
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (length(modelFit$obsLevels) == 2) {
					tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				else {
					tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
							byrow = TRUE)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				tmp_pred <- as.data.frame(tmp_pred)
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$predictor <- function (x, ...) {
		imp <- xgb.importance(x$xNames, model = x)
		x$xNames[x$xNames %in% imp$Feature]
	}
	
	m$varImp <- function (object, numTrees = NULL, ...) {
		imp <- xgb.importance(object$xNames, model = object)
		imp <- as.data.frame(imp)[, 1:2]
		rownames(imp) <- as.character(imp[, 1])
		imp <- imp[, 2, drop = FALSE]
		colnames(imp) <- "Overall"
		imp
	}
	
	m$levels <- function (x) x$obsLevels 
	
	m$sort <- function (x) {
		x[order(x$nrounds, x$alpha, x$lambda), ]
	}
	
	m
}



caret.xgb.tree <- function() {
	m <- list(type =  c("Regression","Classification"),
			library = c("xgboost"),
			label = "eXtreme Gradient Boosting tree booster",
			tags = c("Tree-Based Model", "Ensemble Model",
					"Implicit Feature Selection", "Boosting"))
	
	m$parameters <- data.frame(parameter = c("nrounds","max_depth", "eta", 
					"gamma", "colsample_bytree", "min_child_weight"),
			class = rep("numeric", 6),
			label = c("# Boosting Iterations","Max Tree Depth","Shrinkage",
					"Minimum Loss Reduction", "Subsample Ratio of Columns",
					"Minimum Sum of Instance Weight"))
	
	m$grid <- function (x, y, len = NULL, search = "grid") {
		if (search == "grid") {
			out <- expand.grid(max_depth = seq(1, len), nrounds = floor((1:len) * 
									50), eta = c(0.3, 0.4), gamma = 0, colsample_bytree = c(0.6, 
							0.8), min_child_weight = c(1))
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
	
	m$loop <- function (grid) {
		loop <- ddply(grid, c("eta", "max_depth", "gamma", "colsample_bytree", 
						"min_child_weight"), function(x) c(nrounds = max(x$nrounds)))
		submodels <- vector(mode = "list", length = nrow(loop))
		for (i in seq(along = loop$nrounds)) {
			index <- which(grid$max_depth == loop$max_depth[i] & 
							grid$eta == loop$eta[i] & grid$gamma == loop$gamma[i] & 
							grid$colsample_bytree == loop$colsample_bytree[i] & 
							grid$min_child_weight == loop$min_child_weight[i])
			trees <- grid[index, "nrounds"]
			submodels[[i]] <- data.frame(nrounds = trees[trees != 
									loop$nrounds[i]])
		}
		list(loop = loop, submodels = submodels)
	}
	
	
	m$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
		if (is.factor(y)) {
			if (length(lev) == 2) {
				y <- ifelse(y == lev[1], 1, 0)
				dat <- xgb.DMatrix(as.matrix(x), label = y)
				out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
								gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
								min_child_weight = param$min_child_weight), data = dat, 
						nrounds = param$nrounds, objective = "binary:logistic", 
						...)
			}
			else {
				y <- as.numeric(y) - 1
				dat <- xgb.DMatrix(as.matrix(x), label = y)
				out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
								gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
								min_child_weight = param$min_child_weight), data = dat, 
						num_class = length(lev), nrounds = param$nrounds, 
						objective = "multi:softprob", ...)
			}
		}
		else {
			dat <- xgb.DMatrix(as.matrix(x), label = y)
			out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
							gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
							min_child_weight = param$min_child_weight), data = dat, 
					nrounds = param$nrounds, objective = "reg:linear", 
					...)
		}
		out
	}	
	
	m$predict <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (modelFit$problemType == "Classification") {
			if (length(modelFit$obsLevels) == 2) {
				out <- ifelse(out >= 0.5, modelFit$obsLevels[1], 
						modelFit$obsLevels[2])
			}
			else {
				out <- matrix(out, ncol = length(modelFit$obsLevels), 
						byrow = TRUE)
				out <- modelFit$obsLevels[apply(out, 1, which.max)]
			}
		}
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (modelFit$problemType == "Classification") {
					if (length(modelFit$obsLevels) == 2) {
						tmp_pred <- ifelse(tmp_pred >= 0.5, modelFit$obsLevels[1], 
								modelFit$obsLevels[2])
					}
					else {
						tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
								byrow = TRUE)
						tmp_pred <- modelFit$obsLevels[apply(tmp_pred, 
										1, which.max)]
					}
				}
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$prob <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (length(modelFit$obsLevels) == 2) {
			out <- cbind(out, 1 - out)
			colnames(out) <- modelFit$obsLevels
		}
		else {
			out <- matrix(out, ncol = length(modelFit$obsLevels), 
					byrow = TRUE)
			colnames(out) <- modelFit$obsLevels
		}
		out <- as.data.frame(out)
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (length(modelFit$obsLevels) == 2) {
					tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				else {
					tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
							byrow = TRUE)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				tmp_pred <- as.data.frame(tmp_pred)
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$predictors <- function (x, ...) {
		imp <- xgb.importance(x$xNames, model = x)
		x$xNames[x$xNames %in% imp$Feature]
	}
	
	m$varImp <- function (object, numTrees = NULL, ...) {
		imp <- xgb.importance(object$xNames, model = object)
		imp <- as.data.frame(imp)[, 1:2]
		rownames(imp) <- as.character(imp[, 1])
		imp <- imp[, 2, drop = FALSE]
		colnames(imp) <- "Overall"
		imp
	}	
	
	m$levels <- function (x) x$obsLevels 
	
	m$sort <- function (x) {
		x[order(x$nrounds, x$max_depth, x$eta, x$gamma, x$colsample_bytree, 
						x$min_child_weight), ]
	}
	
	m
}


caret.xgb.rank2 <- function() {
	m <- list(type =  c("Regression"),
			library = c("xgboost", "plyr"),
			label = "eXtreme Gradient Boosting rank:pairwise (new)",
			tags = c("gradient boosting", "Learn2Rank")
			)
	
	m$parameters <- data.frame(parameter = c("nrounds","max_depth", "eta", 
					"gamma", "colsample_bytree", "min_child_weight"),
			class = rep("numeric", 6),
			label = c("# Boosting Iterations","Max Tree Depth","Shrinkage",
					"Minimum Loss Reduction", "Subsample Ratio of Columns",
					"Minimum Sum of Instance Weight"))
	
	m$grid <- function (x, y, len = NULL, search = "grid") {
		if (search == "grid") {
			out <- expand.grid(max_depth = seq(1, len), nrounds = floor((1:len) * 
									50), eta = c(0.3, 0.4), gamma = 0, colsample_bytree = c(0.6, 
							0.8), min_child_weight = c(1))
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
	
	m$loop <- function (grid) {
		loop <- ddply(grid, c("eta", "max_depth", "gamma", "colsample_bytree", 
						"min_child_weight"), function(x) c(nrounds = max(x$nrounds)))
		submodels <- vector(mode = "list", length = nrow(loop))
		for (i in seq(along = loop$nrounds)) {
			index <- which(grid$max_depth == loop$max_depth[i] & 
							grid$eta == loop$eta[i] & grid$gamma == loop$gamma[i] & 
							grid$colsample_bytree == loop$colsample_bytree[i] & 
							grid$min_child_weight == loop$min_child_weight[i])
			trees <- grid[index, "nrounds"]
			submodels[[i]] <- data.frame(nrounds = trees[trees != 
									loop$nrounds[i]])
		}
		list(loop = loop, submodels = submodels)
	}
	
	m$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
		dat <- xgb.DMatrix(as.matrix(x), label = y, group = rep(2, length(y)/2) )
		out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth, 
						gamma = param$gamma, colsample_bytree = param$colsample_bytree, 
						min_child_weight = param$min_child_weight), 
				data = dat, nrounds = param$nrounds, objective = "rank:pairwise", 
				...)
		
		out
	}
	
	m$predict <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$prob <- function (modelFit, newdata, submodels = NULL) {
		newdata <- xgb.DMatrix(as.matrix(newdata))
		out <- predict(modelFit, newdata)
		if (length(modelFit$obsLevels) == 2) {
			out <- cbind(out, 1 - out)
			colnames(out) <- modelFit$obsLevels
		}
		else {
			out <- matrix(out, ncol = length(modelFit$obsLevels), 
					byrow = TRUE)
			colnames(out) <- modelFit$obsLevels
		}
		out <- as.data.frame(out)
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (length(modelFit$obsLevels) == 2) {
					tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				else {
					tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
							byrow = TRUE)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				tmp_pred <- as.data.frame(tmp_pred)
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	m$predictor <- function (x, ...) {
		imp <- xgb.importance(x$xNames, model = x)
		x$xNames[x$xNames %in% imp$Feature]
	}
	
	m$varImp <- function (object, numTrees = NULL, ...) {
		imp <- xgb.importance(object$xNames, model = object)
		imp <- as.data.frame(imp)[, 1:2]
		rownames(imp) <- as.character(imp[, 1])
		imp <- imp[, 2, drop = FALSE]
		colnames(imp) <- "Overall"
		imp
	}
	
	m$levels <- function (x) x$obsLevels 
	
	m$sort <- function (x) {
		x[order(x$nrounds, x$max_depth, x$eta, x$gamma, x$colsample_bytree, 
						x$min_child_weight), ]
	}
	
	m
}


getSparseXGBModel <- function() {
	model.xb <- getModelInfo(model="xgbTree")$xgbTree
	
	model <- model.xb
	
	model$label <- "eXtreme Gradient Boosting for sparse input"
	
	model$fit <- function (x, y, wts, param, lev, last, classProbs, matrix, ...) 
	{
		if ( is.null(x$idx) ) stop("Input x must contain a column named 'idx' counting the rows of matrix.")
		
		if (is.factor(y)) {
			if (length(lev) == 2) {
				y <- ifelse(y == lev[1], 1, 0)
				dat <- xgb.DMatrix(matrix[x$idx,], label = y)
				out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth), 
						data = dat, nrounds = param$nrounds, objective = "binary:logistic", 
						...)
			}
			else {
				y <- as.numeric(y) - 1
				dat <- xgb.DMatrix(matrix[x$idx,], label = y)
				out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth), 
						data = dat, num_class = length(lev), nrounds = param$nrounds, 
						objective = "multi:softprob", ...)
			}
		}
		else {
			dat <- xgb.DMatrix(matrix[x$idx,], label = y)
			out <- xgb.train(list(eta = param$eta, max_depth = param$max_depth), 
					data = dat, nrounds = param$nrounds, objective = "reg:linear", 
					...)
		}
		
		out$matrix <- matrix
		
		out
	}
	
	model$predict <- function (modelFit, newdata, submodels = NULL) 
	{
		newdata <- xgb.DMatrix(modelFit$matrix[newdata$idx,])
		out <- predict(modelFit, newdata)
		if (modelFit$problemType == "Classification") {
			if (length(modelFit$obsLevels) == 2) {
				out <- ifelse(out >= 0.5, modelFit$obsLevels[1], 
						modelFit$obsLevels[2])
			}
			else {
				out <- matrix(out, ncol = length(modelFit$obsLevels), 
						byrow = TRUE)
				out <- modelFit$obsLevels[apply(out, 1, which.max)]
			}
		}
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (modelFit$problemType == "Classification") {
					if (length(modelFit$obsLevels) == 2) {
						tmp_pred <- ifelse(tmp_pred >= 0.5, modelFit$obsLevels[1], 
								modelFit$obsLevels[2])
					}
					else {
						tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
								byrow = TRUE)
						tmp_pred <- modelFit$obsLevels[apply(tmp_pred, 
										1, which.max)]
					}
				}
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	model$prob <- function (modelFit, newdata, submodels = NULL) 
	{
		newdata <- xgb.DMatrix(modelFit$matrix[newdata$idx,])
		out <- predict(modelFit, newdata)
		if (length(modelFit$obsLevels) == 2) {
			out <- cbind(out, 1 - out)
			colnames(out) <- modelFit$obsLevels
		}
		else {
			out <- matrix(out, ncol = length(modelFit$obsLevels), 
					byrow = TRUE)
			colnames(out) <- modelFit$obsLevels
		}
		out <- as.data.frame(out)
		if (!is.null(submodels)) {
			tmp <- vector(mode = "list", length = nrow(submodels) + 
							1)
			tmp[[1]] <- out
			for (j in seq(along = submodels$nrounds)) {
				tmp_pred <- predict(modelFit, newdata, ntreelimit = submodels$nrounds[j])
				if (length(modelFit$obsLevels) == 2) {
					tmp_pred <- cbind(tmp_pred, 1 - tmp_pred)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				else {
					tmp_pred <- matrix(tmp_pred, ncol = length(modelFit$obsLevels), 
							byrow = TRUE)
					colnames(tmp_pred) <- modelFit$obsLevels
				}
				tmp_pred <- as.data.frame(tmp_pred)
				tmp[[j + 1]] <- tmp_pred
			}
			out <- tmp
		}
		out
	}
	
	model
}

