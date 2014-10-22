## The functions incuded in this file are used to cache the results of a matrix 
##   inverse calcuation (an expensive operation) for those cases where the 
##   matrix does not change.  They were created as an assignment for R Programming 
##   (https://class.coursera.org/rprog-008)

## The makeCacheMatrix function creates a list object that exposes the 
##   getter and setter methods on a matrix and its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse of a matrix using the 
##   solve function, caches the result, and returns the newly calcuated 
##   inverse.  It is expected to be used in conjunction with the makeCacheMatrix
##   function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

