## A pair of functions that allows a matrix and its inverse to be stored.
## When the inverse is requested it is retrieved from cache unless the inverse
## of the current matrix has not been calculated before.
## (functions based on examples provided in the Coursera R Programming
## Assignment 2 outline at: 
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping)

## Creates a list of functions that allow access to get/set a matrix and its
## inverse
makeCacheMatrix <- function(x = matrix()) {
    x.inv <- NULL
    
    set <- function(y){
        x <<- y
        x.inv <<- NULL
    }
    get <- function() x
    
    setInverse <- function(x) x.inv <<- x
    getInverse <- function() x.inv
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the cached matrix's inverse, either by retrieving the cached inverse
## or calculating (and caching) it if not already cached
cacheSolve <- function(x, ...) {
        mat.inv <- x$getInverse()
        
        if (!is.null(mat.inv)){
            message("getting cached data")
            return(mat.inv)
        }
        
        mat <- x$get()
        mat.inv <- solve(mat, ...)
        x$setInverse(mat.inv)
        mat.inv
}