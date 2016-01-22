## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can be cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## inverse matrix stores in cache
        mt <- NULL
        
        ## reset the matrix and inverse matrix
        set <- function(y) {
                x <<- y
                mt <<- NULL
        }
        
        ## retrieve the matrix from cache
        get <- function() {
                x
        }
        
        ## store the inverse matrix in cache
        setMatrix <- function(m) {
                mt <<- m
        }
        
        ## retrieve the invese matrix from cache
        getMatrix <- function() {
                mt
        }
        
        ## a list of functions to access matrix and inverse matrix
        list (set = set, get = get, setMatrix = setMatrix, getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the inverse matrix from cache first
        mt <- x$getMatrix()
        if(!is.null(mt)) {
                message("getting cached matrix")
                mt
        } else {
                
                ## inverse matrix does not exist in cache
                ## get the matrix from cache
                data <- x$get()
                ## inverse the matrix
                mt <- solve(data, ...)
                ## store inverse matrix into cache
                x$setMatrix(mt)
                mt
        }
}
