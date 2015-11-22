## This file contains functions to fulfill Coursera R Programming
## Assignment 2.

## makeCacheMatrix creates a cache of the inverse of the matrix
## that the user inputs to the function itself.

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
      x <<- y
      iv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) iv <<- inverse
    getinverse <- function() x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Use output of makeCacheMatrix to either return the matrix
## or its inverse.

cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if(!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  iv <- solve(data, ...)
  x$setinverse(iv)
  iv
}
