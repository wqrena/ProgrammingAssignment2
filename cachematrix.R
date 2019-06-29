#------------------------------------------------------------------------------------------------------------------------
#                               Rprogramming_Week3_Assignement2
# 
# There are two funcitons below. The first function makeCacheMatrix is to create R objects 
# (such as a matrix, its inverse matrix and a list containing 4 functions) in the makeCacheMatrix() object's environment.
# The second function cacheSolve uses the returned value from makeCacheMatrix as argument to retrieve
# the inverse matrix from the cached value stored in makeCacheMatrix() object's environment. If there is no cached value,
# the inverse matrix is calculated and returned, otherwise the cached value will be returned.
#------------------------------------------------------------------------------------------------------------------------



# Initiate matrix and its inverse matrix
# Return a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# Retrieve inverse matrix using functions that are created from the returned list of makeCacheMatrix.
# If no cacehed value, recalcualte the inverse matrix

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
