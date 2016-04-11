## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	     inversion <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(inverse) inversion <<- inversion
        getinversion <- function() m
        list(set = set, get = get,
             setinversion = setinversion,
             getinversion = getinversion)	
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inversion <- x$getinversion()
        if(!is.null(inversion)) {
                message("getting cached data")
                return(inversion)
        }
        mydata <- x$get()
        inversion <- solve(mydata, ...)
        x$setinversion(inversion)
        inversion
}
