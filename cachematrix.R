## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## As far as I understand it, the logic of this program is as follows.
## “Caching” refers to using a value that is already defined rather than
## re-calculating it. This can be done by using the scoping rules and in
## particular the <<- operator.
##
## First, makeCacheMatrix() creates a list of functions, which can be used to
## set the value of the matrix and retrieve them, as well as get the result
## of solve() (and provide one?).
##
## makeCacheMatrix returns a list.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolution <- function(solution) s <<- solution
    getsolution <- function() s
    list (set = set, get = get,
          setsolution = setsolution,
          getsolution = getsolution)
}

## Write a short comment describing this function
##
## cacheSolve takes an argument of the type of the output of makeCacheMatrix
## as an argument and tries to see whether that list's getsolution() function
## is null or not. If it is not, it will not calculate the inverse but simply
## return the pre-set (cached) value.
##
## If it is null (e.g. is.null(s) is TRUE), it will solve the matrix using
## the makeCacheMatrix function defined above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolution()
    if (!is.null(s)) {
        message("Getting cached data.")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolution(s)
    s
}
