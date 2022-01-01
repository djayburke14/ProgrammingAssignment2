## Two functions are created. The first function makeCacheMatrix() takes a
## matrix (x) as its only argument and returns a list containing four functions.
## These four functions do the following:
##      1. set() reassigns the variable x to a new matrix
##      2. get() gets the value of x from the 'cache' (the environment 
##         associated with makeCacheMatrix())
##      3. set_inverse() sets the value of the inverse matrix of x
##      4. get_inverse() gets the inverse matrix of x from the cache
## The second function, cacheSolve(), takes an output from makeCacheMatrix() as 
## an argument. It then determines if an inverse matrix of the given matrix 
## already exists in the cache. If so, the existing inverse matrix is returned.
## Otherwise, a new inverse matrix is calculated and returned.


## The following function creates a list of functions that can be used to 
## cache a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse <- function(inv_matrix) {
        inverse <<- inv_matrix
    }
    get_inverse <- function() {
        inverse
    }
    list(set = set, get = get, set_inverse = set_inverse, 
         get_inverse = get_inverse)
}


## The following function either retrieves a cached inverse matrix and
## returns it, or calculates the inverse of a matrix, caches it, and returns it.

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if (!is.null(inverse)) {
        message("Getting cached inverse matrix...")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}
