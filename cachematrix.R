## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes an invertible matrix and returns a list of functions that include setting the matrix,
## retrieving the values of the matrix, setting the value of its inverted matrix, and returning its inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    setM <- function(mat) {
        x <- mat
        i <<- NULL
    }
    getM <- function() x
    seti <- function(solved) i <<- solved
    geti <- function() i
    list(setM = setM, getM = getM, seti = seti, geti = geti)
}


## Write a short comment describing this function
## cacheSolve takes a special list of functions from makeCacheMatrix as an argument.
## cacheSolve looks in the cache/parent environment of a matrix to see if the inverted matrix exists.
## if the inverted matrix i is in the cache, it retrieves and returns the inverted matrix, otherwise
## it calculates the inverted matrix then stores it into cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$geti()
    if(!is.null(i)) {
        message("getting cached inverse")
        ##print("getting cached inverse")
        return(i)
    }
    data <- x$getM()
    i <- solve(data, ...)
    x$seti(i)
    i
}
