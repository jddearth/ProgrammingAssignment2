## This second programming assignment will require you to write an R function that is 
## able to cache potentially time-consuming computations. If the contents of a vector 
## are not changing, it makes sense to cache the value of the mean so that when we 
## need it again, it can be looked up in the cache rather than recomputed.

## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. Your assignment 
## is to write a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix creates a list containing a function to:
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of inverse of the matrix
        ## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
} 

## Return a matrix that is the inverse of 'x'
