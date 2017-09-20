## Matrix inversion is usually a costly process. These functions
## create a special matrix object that can cache the inverse 
## once it is calculated for the first time.

## This creates a special matrix object that caches the matrix
## itself and its inverse as well

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function gets the inverse of the given matrix from the
## cache if it alredy exists. Else, it calculates the inverse
## and sets it to the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invertedX <- x$getinv()
    if(!is.null(invertedX) & identical(invertedX,x)) {
        return(invertedX)
    }
    data <- x$get()
    invertedX <- solve(data, ...)
    x$setinv(invertedX)
    invertedX
}
