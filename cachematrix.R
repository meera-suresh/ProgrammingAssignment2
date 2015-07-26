## Caching the Inverse of a Matrix

## This function will create a matrix object that will then be cached as its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(ss) m <<- ss
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function computes the inverse of the matrix created in the previous function "makeCacheMatrix". If the inverse has already been calculated with no changes to the matrix, then "cacheSolve" will retrieve the inverse from the cache instead of computing it again.

cacheSolve <- function(x=matrix, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
