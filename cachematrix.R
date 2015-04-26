## Create an object that can cacheing of the results of a matrix inversion.


## Create the object, storing the value of the matrix internally
## Don't do the inversion yet - that will be handled by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
	mInvertCached <- NULL
  ## If the user assigns a new value to the matrix, replace the internal value for x
  ## and null the cached calculation so that the next time getInvert is called it will
  ## return null and prompt a new calculation
	set <- function(mNew) {
		x <<- mNew
		mInvertCached <<- NULL
	}
	get <- function() x
	setInvert <- function(mInvert) mInvertCached <<- mInvert
	getInvert <- function() mInvertCached
	list(set = set, get = get,
		 setInvert = setInvert,
		 getInvert = getInvert)
}


## Try to fetch the cached inverse value.
## If not found, do the calculation and store that value in the cache
## then return the inverse value
cacheSolve <- function(x, ...) {
  ## NOTE - This does not store the additional parameters in the cache!
  ##        Calling cacheSolve(m), then calling CacheSolve(m, 3) will return the cached value
  ##        Since caching the additional parameters was not specified in the assignment, I 
  ##        will ignore this shortcoming.
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
