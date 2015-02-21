## Put comments here that give an overall description of what your
## functions do

## Prepares a matrix for cached inversion. The result of makeCacheMatrix is used as input for cacheSolve
##  x is a matrix whose inverse we wish to compute and cache
makeCacheMatrix <- function(x = matrix()) {
    #we initialize the inverse as NULL so that we know when it has or hasn't been computed
    i <- NULL
    
    #used to set the value of the underlying matrix. If we ever use this then we have to reset
    #  the inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    #returns the underlying matrix
    get <- function() x
    
    #sets the inverse value
    setinverse <- function(inverse) i <<- inverse
    
    #returns the value of the stored inverse
    getinverse <- function() i
    
    #return the cacheMatrix object
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
#  x must be a result of running makeCacheMatrix on the original matrix that needs to be inverted
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    # if the inverse was already cached just get it from cached value and return it
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # if the inverse hasn't already been calculated we compute it here, store it and return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
