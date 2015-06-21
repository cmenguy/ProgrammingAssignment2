## Wrapper around a matrix to provide functionality
## to cache its inverse to save compute time
makeCacheMatrix <- function(x = matrix()) {
    # Input validation, just in case
    if (!is.matrix(x)) {
        stop("Invalid parameter, expected matrix")
    }
    
    invertedmatrix <- NULL
    
    set <- function(y) {
        x <<- y
        invertedmatrix <<- NULL
    }
    
    # Functions for getting and setting cached inv. matrix value
    get <- function() x
    # Inverting the matrix using built-in solve() function in R
    setinverse <- function(solve) invertedmatrix <<- solve
    getinverse <- function() invertedmatrix
    
    list(
        set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the inverse of a matrix by either
## computing it, or getting it from the cache
## if it was already computed
cacheSolve <- function(x, ...) {
    invertedmatrix <- x$getinverse()
    # If inverse available, return it to save time
    if(!is.null(invertedmatrix)) {
        message("Getting cached inverse matrix")
        return(invertedmatrix)
    }
    # If inverse not available, compute it,
    # cache it and return it
    data <- x$get()
    invertedmatrix <- solve(data)
    x$setinverse(invertedmatrix)
    invertedmatrix
}
