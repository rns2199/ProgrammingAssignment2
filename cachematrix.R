## The below functions provide the capability to cache the inverse of 
## a matrix in order to reduce the cost when the inverse needs to 
## be computed multiple times.

## The makeCacheMatrix function returns a data structure that
## contains closure functions that hold the matrix and inverse
## matrix as well as functions to set new matrix and set the 
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    ## Create and return the list data structure containing the functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function returns the inverse of a matrix.
## The argument to the function must be the data structure
## returned from makeCacheMatrix().  If there is a
## cache hit, then the cached inverse matrix is returned.
## If there is a cache miss, then the inverse matrix
## is computed, set and returned.  The function allows
## for additional paramaters to be passed through to 
## the solve function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
