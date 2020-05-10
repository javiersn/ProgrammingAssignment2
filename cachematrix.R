## Functions in this file will invert a matrix using
## cache to be more efficient

## Creates a special vector that allows getting and
## setting the matrix and the inversion.

makeCacheMatrix <- function(m = matrix()) {
    i <- NULL
    set <- function(n) {
        m <<- n
        i <<- NULL
    }
    get <- function() m
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Checks if the inverted matrix is cached, if not it
## sets the inverse of the matrix and caches it.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
