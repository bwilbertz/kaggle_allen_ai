# TODO: Add comment
# 
# Author: benedikt
###############################################################################


t2vec <- function(tokens, h, ncol) {
	df <- data.frame(matrix(ncol = length(tokens), nrow = ncol))
	colnames(df) <- make.names(tokens)
	i <- 1
	for ( str in tokens ) {
		if ( has.key(str, h)) v <- h[[str]] else v <- numeric(ncol)
		df[,i] <- v
		i =i+1
	}
	
	df
}

cross_prod <- function(df.a, df.b, k) {
	ans <- apply(df.a, 2, .inner_cross, df=df.b, k=k)

	unlist(ans)
}

.inner_cross <- function(a, df.b, k) {
	ans <- apply(df.b, 2, k, a)

	unlist(ans)
}

sigmoid_kernel <- function(a, b) {
	sigmoid(dot_prod_nan(a,b))
}

similarity_kernel <- function(a, b) {
	dot_prod_nan(a/norm_vec(a),b/norm_vec(b))
}

norm_vec <- function(x) sqrt(sum(x^2))

dot_prod <- function(a,b) sum(a*b)

dot_prod_nan <- function(a,b) {
	d <- dot_prod(a,b)
	
	if ( is.nan(d) ) 
		return(0)
	
	d
}

sigmoid <- function(x) 1. / (1. + exp(-x))  

normalize_vec <- function(x) {
	len = sqrt(sum(x^2))
	
	if ( len == 0 )
		return(numeric(length(x)))
	
	x / len
}


cross_idf.geo <- function(t.a, t.b, h) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))

	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)

	for (i in seq_along(t.a)) {
		if ( has.key(t.a[i], h)) v <- h[[t.a[i]]] else v <- 1
		for (j in seq_along(t.b)) {
			if ( has.key(t.b[j], h)) w <- h[[t.b[j]]] else w <- 1
			m[j,i] <- sqrt(v*w)
		}
	}

	m
}

cross_idf.avg <- function(t.a, t.b, h) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))

	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	for (i in seq_along(t.a)) {
		if ( has.key(t.a[i], h)) v <- h[[t.a[i]]] else v <- 1
		for (j in seq_along(t.b)) {
			if ( has.key(t.b[j], h)) w <- h[[t.b[j]]] else w <- 1
			m[j,i] <- 0.5*(v+w)
		}
	}

	m
}

.idf <- function(t, h) {
	m <- vector('numeric', length(t))

	names(m) <- make.names(t)

	for (i in seq_along(t)) {
		if ( has.key(t[i], h)) v <- h[[t[i]]] else v <- 1
		
		if ( length (v) == 0 ) {
			warning(paste("Cannot find idf for:",t[i]))
			m[i] <- 1.0
		} else {
			m[i] <- v
		}
	}

	m
}

tcm.cooc <- function(tcm, t.a, t.b, h) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))
	
	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	
	for (i in seq_along(t.a)) {		
		idx.a <- h[[t.a[i]]]
		for (j in seq_along(t.b)) {
			idx.b <- h[[t.b[j]]]
			if ( idx.a < idx.b ) m[j,i] <- tcm[idx.a, idx.b] else m[j,i] <- tcm[idx.b, idx.a]
			if ( t.a[[i]] == 'be' || t.b[[j]] == 'be') m[j,i] <- NA
		}
	}
	
	m
}

f.bm25 <- function(x) 2.2*x / (1+x)

tcm.coProb <- function(tcm, t.a, t.b, h, inv = F, bm25 = F) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))
	
	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	
	for (i in seq_along(t.a)) {		
		idx.a <- h[[t.a[i]]]
		for (j in seq_along(t.b)) {
			idx.b <- h[[t.b[j]]]
			if ( idx.a < idx.b ) m[j,i] <- tcm[idx.a, idx.b] else m[j,i] <- tcm[idx.b, idx.a]
			
			if ( bm25 ) m[j,i] <- 2.2*m[j,i] / (1+m[j,i])			
			
			if ( inv ) m[j,i] <- m[j,i] / (sum(tcm[idx.b,])+sum(tcm[,idx.b])) else m[j,i] <- m[j,i] / (sum(tcm[idx.a,])+sum(tcm[,idx.a])) 
			
			#if ( t.a[[i]] == 'be' || t.b[[j]] == 'be') m[j,i] <- NA
		}
	}
	
	m
}

tcm.coProb.miss <- function(tcm, t.a, t.b, h, inv = F, bm25 = F) {
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

tcm.coProb.bm25 <- function(tcm, t.a, t.b, h, inv = F) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))
	
	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	
	for (i in seq_along(t.a)) {		
		idx.a <- h[[t.a[i]]]
		for (j in seq_along(t.b)) {
			idx.b <- h[[t.b[j]]]
			if ( idx.a < idx.b ) m[j,i] <- tcm[idx.a, idx.b] else m[j,i] <- tcm[idx.b, idx.a]
			
			m[j,i] <- f.bm25(m[j,i])

			if ( inv ) m[j,i] <- m[j,i] / (sum(f.bm25(tcm[idx.b,]))+sum(f.bm25(tcm[,idx.b]))) else m[j,i] <- m[j,i] / (sum(f.bm25(tcm[idx.a,]))+sum(f.bm25(tcm[,idx.a]))) 
			
			#if ( t.a[[i]] == 'be' || t.b[[j]] == 'be') m[j,i] <- NA
		}
	}
	
	m
}

tcm.coProb.dt <- function(dt, t.a, t.b, h, inv = F, bm25 = F) {
	m <- matrix(ncol = length(t.a), nrow = length(t.b))
	
	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	
	for (i in seq_along(t.a)) {		
		idx.a <- h[[t.a[i]]]
		
		if ( is.null(idx.a)) {
			warning(paste("No co-occ for word", t.a[i]))
			next
		}
		#print(t.a[i])
		idx.a = idx.a-1
		
		for (j in seq_along(t.b)) {
			idx.b <- h[[t.b[j]]]
			
			idx.b <- h[[t.b[j]]]
			
			if ( is.null(idx.b)) {
				warning(paste("No co-occ for word", t.b[j]))
				next
			}
			
			idx.b = idx.b-1
			
			if ( idx.a == idx.b ) next
			
			
			dt.ai <- dt[i == idx.a]
			dt.aj <- dt[j == idx.a]
			
			dt.bi <- dt[i == idx.b]
			dt.bj <- dt[j == idx.b]
			
			if ( idx.a < idx.b ) {
				z <- dt.ai[j == idx.b]
				if (nrow(z) == 0 ) next;
				
				m[j,i] <- z$x	
			} else {
				z <- dt.bi[j == idx.a]
				if (nrow(z) == 0 ) next;
				
				m[j,i] <- z$x				
			} 
			
			
			if ( bm25 ) m[j,i] <- 2.2*m[j,i] / (1+m[j,i])			
			
			if ( inv ) m[j,i] <- m[j,i] / (sum(dt.bi$x)+sum(dt.bj$x)) else m[j,i] <- m[j,i] / (sum(dt.ai$x)+sum(dt.aj$x)) 
			
			#if ( t.a[[i]] == 'be' || t.b[[j]] == 'be') m[j,i] <- NA
		}
	}
	
	m
}

tcm.pmi.dt <- function(dt, t.a, t.b, h, bm25 = F, nWords = 50000) {
	m <- matrix(data = 0, ncol = length(t.a), nrow = length(t.b))
	
	colnames(m) <- make.names(t.a)
	rownames(m) <- make.names(t.b)
	
	for (i in seq_along(t.a)) {		
		idx.a <- h[[t.a[i]]]
		
		if ( is.null(idx.a)) {
			warning(paste("No co-occ for word", t.a[i]))
			next
		}
		#print(t.a[i])
		idx.a = idx.a-1
		
		for (j in seq_along(t.b)) {
			idx.b <- h[[t.b[j]]]
			
			if ( is.null(idx.b)) {
				warning(paste("No co-occ for word", t.b[j]))
				next
			}
			
			idx.b = idx.b-1
			
			if ( idx.a == idx.b ) next
			
			#print(t.b[j])
			
			dt.ai <- dt[i == idx.a]
			dt.aj <- dt[j == idx.a]
			
			dt.bi <- dt[i == idx.b]
			dt.bj <- dt[j == idx.b]
			
			if ( idx.a < idx.b ) {
				z <- dt.ai[j == idx.b]
				if (nrow(z) == 0 ) next;
				
				m[j,i] <- z$x	
			} else {
				z <- dt.bi[j == idx.a]
				if (nrow(z) == 0 ) next;
				
				m[j,i] <- z$x				
			} 
			
			if ( bm25 ) {
				m[j,i] <- f.bm25(m[j,i])
				p.a <- (sum(f.bm25(dt.ai$x))+sum(f.bm25(dt.aj$x))) / nWords
				p.b <- (sum(f.bm25(dt.bi$x))+sum(f.bm25(dt.bj$x))) / nWords
			} else {
				p.a <- (sum(dt.ai$x)+sum(dt.aj$x)) / nWords
				p.b <- (sum(dt.bi$x)+sum(dt.bj$x)) / nWords
			}
			
			m[j,i] <- m[j,i] / (p.a*p.b) 
			
			#if ( t.a[[i]] == 'be' || t.b[[j]] == 'be') m[j,i] <- NA
		}
	}
	
	m
}

