## Pair of functions that cache the inverse of a matrix.
## To benefit from caching the inverse of a matrix rather than computing it repeatedly

## function that creates a special "matrix" object that can cache its inverse.
# x: invertible square matrix object
# Returns a vector containing all the related functions
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL # the inverse parameter must be reset
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function that computes the inverse of the special "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# x: square matrix object created with makeCacheMatrix
# Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    matrix <- x$get()
    i <- try(solve(matrix))
    x$setinverse(i)
    i
}
