## This pair of functions will compute the inverse of a matrix
## using caching. Because matrix inversion is computationally
## expensive, caching will allow it to be executed much faster.


## This function creates a special matrix that can cache
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function returns the inverse of the matrix created by
## the 'makeCacheMatrix' function. If the inverse has already
## been computed and the matrix is unchanged, this should
## simply retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
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