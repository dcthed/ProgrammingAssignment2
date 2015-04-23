## These two functions create and retrieve a matrix whose 
## inverse can be calculated, cached, and retrieved (as well).


## makeCacheMatrix stores a matrix and retrieves its inverse if it
## is cached.

## The function input (x) is a square matrix that can be inverted.
## inv (short for inverse) is set NULL when the function is called
## x$set allows for overwriting the matrix from the original function call
## x$get retreives the matrix from the original function call or x$set cache
## x$setinv caches the matrix inverse
## x$getinv retreives the cached inverse (if called before cacheSolve() returns NULL)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes a matrix input (from makeCacheMatrix); if the inverse has
## already been calculated, cacheSolve() retreives the cached inversed matrix
## using the x$getinv() function from makeCacheMatrix.  If the inversed matrix
## has not been calculated (cached), cacheSolve() calculates and caches the 
## inverse-matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Retrieving cached inversed matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}