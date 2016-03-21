## this functions intent to reduce computations of matrix inverse
## by using cache

## This function creates "special matrix" and return list that is used
## as an input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        cached_m <- NULL
        set <- function(some_matrix) {
                x <<- some_matrix
                cached_m <<- NULL
        }
        get <- function() x
        setsolve <- function(my_solve) cached_m <<- my_solve
        getsolve <- function() cached_m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)        
}


## this function computes inverse of matrix, but first it checks if there
## is or not already computed matrix in cache

cacheSolve <- function(x, ...) {
        cached_m <- x$getsolve()
        if (!is.null(cached_m)) {
                message("pulling matrix from cache")
                return(cached_m)
        }
        my_data <- x$get()
        cached_m <- solve(my_data, ...)
        x$setsolve(cached_m)
        return(cached_m)
        
        ## Return a matrix that is the inverse of 'x'
}
