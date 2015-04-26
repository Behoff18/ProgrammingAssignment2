## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	mInvertCached <- NULL
	mOriginal     <- x 
	set <- function(mNew) {
		mOriginal <<- mNew
		mInvertCached <<- NULL
	}
	get <- function() mOriginal
	setInvert <- function(mInvert) mInvertCached <<- mInvert
	getInvert <- function() mInvertCached
	list(set = set, get = get,
		 setInvert = setInvert,
		 getInvert = getInvert)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	mInvert <- x$getInvert()
	if(!is.null(mInvert)) {
		message("getting cached data")
		return(mInvert)
	}
	data <- x$get()
	mInvert <- solve(data, ...)
	x$setInvert(mInvert)
	mInvert

}
