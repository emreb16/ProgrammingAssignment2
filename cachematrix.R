## This code was prepared for R programming on Coursera.org 
## These functions use lexical scoping to take a square invertible matrix and cache the inverse in order 
## to reduce the need to go through costly computations to find the matrix inverse.


## This function takes a square invertible matrix as input and looks for a cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function finds the cached value for the inverse of the matrix defined previously.If there is no cached inverse 
## from makeCacheMatrix, then the function cacheSolve solves for the inverse and caches the output for future use.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
