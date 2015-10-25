## Use makeCacheMatrix to create a special matrix that can cache its inverse matrix;
## Use cacheSolve to get the inverse matrix.
# Example of use (use dim 2 unit matrix, whose inverse is itself):
# > source('cachematrix.R')
# > m<-matrix(c(1,0,0,1), c(2,2))
# > m
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > sm<-makeCacheMatrix(m)
# > cacheSolve(sm)
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > cacheSolve(sm)
# Returning cached data ...
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1

## Special matrix, wrapper around a normal matrix that caches an inverse
makeCacheMatrix <- function(x = matrix()) {
    # The inverse matrix
    inv <- NULL
    
    # Set a matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get the matrix
    get <- function() x
    
    # Set/cache inverse matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Get the cached inverse matrix
    getInverse <- function() inv
    
    # Expose the belly of the beast
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Interface to the special matrix; it computes the inverse matrix if not already cached
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Returning cached data ...")
        return(inv)
    }
    # If there is no cached inverse matrix
    # Get the matrix
    matrix <- x$get()
    
    # Invert it
    inv <- solve(matrix)
    
    # Cache inverse matrix
    x$setInverse(inv)
    
    # Return the inverse matrix
    inv
}
