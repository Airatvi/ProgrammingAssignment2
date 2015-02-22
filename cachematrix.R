## This is functions which allow calculate inverse matrix 
## with caching last result

## create place where will store our matrix and cache

makeCacheMatrix <- function(x = matrix()) {
	m<-matrix(nrow = 0, ncol = 0)
	set<-function(y)
	{
		x<<-y
		m<<-matrix(nrow = 0, ncol = 0)
	}
	get<-function() x
	getm<-function() m
	setm<-function(sl) m<-sl
	list(set = set, get = get,
	setm = setm,
	getm = getm)
}


## examine cache, calculate inverse, and complete cache

cacheSolve <- function(x, ...) {
	m<-x$getm()
	if (length(m) != 0) {
		return (m)
	}
	result<-solve(x$get())
	x$setm(result)
	result
        ## Return a matrix that is the inverse of 'x'
}
