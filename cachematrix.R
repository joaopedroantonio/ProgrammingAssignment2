## These functions allow caching the results of the calculation of
## the inverse of a matrix, avoiding repeated calculations for the
## same matrix. The inverse of a matrix is kept in memory next to
## the matrix itself and accessed whenever necessary, instead of
## being re-calculated several times.

## Creates an object containing the matrix passed as argument
## and able to store a the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the matrix passed as argument,
## assuming that matrix was created using the 'makeCacheMatrix' function
## If the inverse of the matrix was calculated previously,
## returns the cached value
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("cache")
        return(i)
    }
    matrix <- x$get()
    i <- solve(matrix, ...)
    x$setinverse(i)
    i
}
