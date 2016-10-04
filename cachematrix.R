# The assignment requires to write two separate functions makeCacheMatrix and cacheSolve
# which seems to me to be an awkward implementation of the memoization pattern
# It would be handier to compute an inverse directly in the set function of makeCacheMatrix
# this would allow to get rid of setInverse function


# Matrix that can be bound with its inverse
makeCacheMatrix <- function(M = matrix()) {
    # memoization field
    M_inverse <- NULL

    # setter and getter "methods" for the matrix M   
    set <- function(m) {
        M <<- m
        M_inverse <<- NULL
    }
    get <- function() { M }

    # setter and getter "methods" for the inverse of M
    setInverse <- function(inverse) { M_inverse <<- inverse }
    getInverse <- function() { M_inverse }

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Computes an inverse of a matrix if it's null, otherwise return the cached value
cacheSolve <- function(M, ...) {    
    inverse <- M$getInverse()    
    if (!is.null(inverse)) {
        # instead of a message a flag is returned
        # this will also allow to test the caching
        return(list(cached = 1, inverse = inverse))
    }
    m <- M$get()
    inverse <- solve(m, ...)
    M$setInverse(inverse)
    list(cached = 0, inverse = inverse)
}

# A simple unit test
# Test matrix is fixed as it's known to be invertible
unitTestMatrixInverse <- function() {
    # create a cacheable matrix
    mx <- makeCacheMatrix(matrix(1:4, 2, 2))

    # first call must perform computation of inverse
    inverse <- cacheSolve(mx)
    if (inverse$cached == 1) {
        message("Expected a newly computed inverse")
        stop()
    }

    # second call must retrive the inverse from cache
    inverse <- cacheSolve(mx)
    if (inverse$cached == 0) {
        message("Expected a cached inverse")
        stop()
    }

    message("Success")
}
