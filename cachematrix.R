## Functions that inplement caching for computing inverse matrix


## Creates a list that represets a matrix with cached inverse
##    m  -  an initial matrix
makeCacheMatrix <- function(m = matrix()) {
    inv <- NULL
    list(

        # Set a new matrix value
        set = function (new.m) {
            m <<- new.m
            inv <<- NULL
        },

        # Get wrapped matrix value
        get = function () {
            m
        },
 
        # Run solve() function on wrapped matrix value
        solve = function (...) {
            if (is.null(inv)) {
                inv <<- solve(m, ...)
            }
            inv
        }
    )
}


## Computes the inverse of a matrix wrapped by makeCacheMatrix
##    wm  - a wrapped matrix,
##    ... - other parameters to be passed to solve() function
cacheSolve <- function(wm, ...) {
    wm$solve(...)
}
