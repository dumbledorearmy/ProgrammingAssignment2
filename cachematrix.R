## Memory management in R - Matrix Inversion and cache management
## This function creates a special "matrix" object that can cache its inverse
## Initializes the matrix and functions get, set, getinverse and setinverse
## which are then used later in the function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
## This function checks if inverse has already been calculated ("cached"), and if not, calculates the inverse
## and returns the inverse matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
