## This file contains 2 functions: makeCacheMatrix and cacheSolve

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        ## Initialise m
        m <- NULL

        ## Set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## Get the matrix
        get <- function() {
                ## Return the matrix
                x
        }

        ## Set the inverse of the matrix
        setInverse <- function(inverse) {
                m <<- inverse
        }

        ## Get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse of the matrix
                m
        }

        ## Return the list of the methods      
        list(set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix function above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve function should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()

        ## If the inverse is already set, return the inverse
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        ## Get the matrix
        data <- x$get()

        ## Compute the inverse of the matrix using the solve function
        m <- solve(data, ...)

        ## Set the inverse of the matrix to the object
        x$setInverse(m)

        ## Return the inverse of the matrix      
        m
}
