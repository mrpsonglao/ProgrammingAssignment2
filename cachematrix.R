### GENERAL DESCRIPTION:
## The pair of functions below named "makeCacheMatrix" and "cacheSolve"
##  are meant to be used together to compute and cache the inverse
##  of a given invertible matrix.

## The rationale for caching the computed matrix inverses is to save time
## and computing power, especially for inverses which are repeatedly computed.

### Function 1: "makeCacheMatrix" function
## The "makeCacheMatrix" function will receive a matrix as its input and will
## output a list of functions which can cache the inverse of the inputted matrix.
## Note: The inputted matrix is assumed to be always invertible.

makeCacheMatrix <- function(x = matrix()) {
    m <- matrix(NA)
    set <- function(y){
        x <<- y
        m <<- matrix(NA)
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

### Function 2: "cacheSolve" function
## The "cacheSolve" function computes the inverse of the matrix inputted into
## the "makeCacheMatrix" function above. If the inverse has been previously
## calculated, this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.na(m[1,1])){
        message("getting cached data")
        return (m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}