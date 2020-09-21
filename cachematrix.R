## The file contains the makeCacheMatrix and cacheSolve functions.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.




## makeCacheMatrix returns a list containing four functions. The formal argument
## should be a square and invertible matrix, the default argument x is a 1 by 1 
## matrix with NA content. x$set(A) and x$setinv(inv) are the functions that can 
## cache the matrix (square and invertible A) and its inverse (inv).
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        
        set <- function(A) {
                x <<- A
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        
        list( set = set, get = get,
              setinv = setinv,
              getinv = getinv)
}

cacheSolve <- function(x, ...) {
        I <- x$getinv()
        
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I ## Return a matrix that is the inverse of 'x'
}