## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## The first function, makeCacheMatrix creates a special "matrix", 
    ## which is really a list containing a function to:
    
        ## set the value of the matrix
        ## get the value of the matrix
        ## set the value of the inverse
        ## get the value of the inverse
    
    ## Set inverse to NULL
    inverse <- NULL
    
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    getMatrix <- function() x
    
    setInverse <- function(z) inverse <<- z
    
    getInverse <- function() inverse
    
    ## Save into a list
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

## This function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    
    ## It checks if the inverse of the matrix has already been calculated
    if(!is.null(inverse)) {
        message("Get cached matrix")
        return(inverse)
    }
    
    data <- x$getMatrix()
    
    ## Solve returns the inverse of a matrix
    inverse <- solve(data, ...)
    
    x$setInverse(inverse)
    
    return(inverse)
}
