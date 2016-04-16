## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	     inversion <- NULL
        set <- function(y) {
                x <<- y
                inversion <<- NULL
        }
        get <- function() x
        setinversion <- function(inverse) inversion <<- inverse
        getinversion <- function() inversion
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inversion <- x$getinversion()
        if(!is.null(inversion)) {  ## If the inverse has already been calculated (and the matrix has not changed)
                message("getting cached data")
                return(inversion)  ## Retrieve the inversion from the cache.
        }
        mydata <- x$get()
        inversion <- solve(mydata, ...)
        x$setinversion(inversion)
        inversion
}
