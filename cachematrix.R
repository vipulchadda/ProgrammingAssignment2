## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
