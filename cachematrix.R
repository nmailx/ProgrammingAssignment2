## The 2 functions in this R file can be used to cache the inverse of a matrix.
## It is assumed that the matrix input is invertible.

## makeCacheMatrix takes in one matrix as argument. If the input data type is
## not matrix, it is converted to a matrix.
## makeCacheMatrix returns a list of 4 functions that can cache the inverse:
## set(y) allows the user to alter/set the cached  matrix
## get() returns the cached matrix
## setinv(inversem) allows the user to the alter/set the cached inverse matrix
## getinv() returns the cached inverse matrix
##
## Example: var <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## var$getinv() would return NULL as the inverse has not been calculated yet,
## so there is no cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invm <- NULL
    
    set <- function(y) {
        x <<- y
        invm <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inversem = matrix()) invm <<- inversem
    getinv <- function() invm
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes in the list returned by makeCacheMatrix and calculates the
## inverse. If the inverse has already been calculated i.e. getinv() not NULL, 
## then cacheSolve retrieves the inverse from the cache. A new calculation will 
## be done If the inverse has not been calculated yet, or if the matrix has been
## changed (which would change the cached inverse to NULL anyway).
##
## Example (var from above): cacheSolve(var) the first time returns the inverse
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(var) the second time will return the message "Getting cached 
## inverse" since there is now a cached inverse in var, and the cached inverse
## is returned instead of repeating the calculation.

cacheSolve <- function(x,...) {
    invm <- x$getinv()
    if (!is.null(invm)) {
        message("Getting cached inverse")
        return(invm)
    }
    ans <- x$get()
    invm <- solve(ans)
    x$setinv(invm)
    invm
}
