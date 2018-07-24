## R Programming Class Assignment 2
## This is a pair of functions that cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {  # Checks to see if m is already cached and returns m if it is
    message("getting cached data")
    return(m)
  }
  data <- x$get() # Calculates the inverse if the code gets to this point
  m <- solve(data, ...)
  x$setinv(m)
  m  # Returns the newly calculated inverse matrix
}
