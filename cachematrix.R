## cachematrix.R
## source("cachematrix.R") - usage
## myMatrix <- makeCacheMatrix(matrix(0:4, 4, 4)
## myMatrix$getMatrix()
## myMatrix$getCache() - will return NULL for the first time
## cacheSolve(myMatrix)
## To view the solution use - myMatrix$getCache() 

makeCacheMatrix <- function(x = matrix()) {

cacheMatrix <- NULL

# set the matrix - setMatrix method
setMatrix <- function(y)
{
	x <<- y
	cacheMatrix <<- NULL
}

#define getMatrix now

getMatrix <- function() x

# Next, setCache

setCache <- function(inverse) cacheMatrix <<- inverse

# Now, defining the getCache to return the cached inverse of x

getCache <- function() cacheMatrix

# as per the makevector example, now list the names of all methods

list(setMatrix = setMatrix,
	getMatrix = getMatrix,
	setCache = setCache,
	getCache = getCache)

## end


}


## To solve and Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
        cacheMatrix <- x$getCache()

# handle null values
	if(!is.null(cacheMatrix)) {
		message("Processing...")
		return(cacheMatrix)
      }
	else {
		nMatrix <- x$getMatrix()
		cacheMatrix <- solve(nMatrix, ...)
		x$setCache(cacheMatrix)
		return(cacheMatrix)
	}
}


