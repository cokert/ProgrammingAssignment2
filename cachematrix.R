## These function implement a wrapper on a matrix that will cache the calculation of its inverse.
##  Example of use: 
##      mc <- makeCache(m)  # where m is an invertible matrix
##      cacheSolve(mc)      # note that the return from the call to makeCache is passed to 
##                          # cacheSolve, NOT the matrix m

## returns a wrapper on a matrix that provides facilities for storing its inverse
##  Functions available: 
##    set(m):       change the matrix that's saved (invalidates the cache, if there is one)
##    get():        return the stored matrix
##    setsolve(m):  stores the the inverted matrix
##    getsolve():   returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function is used to simplify the use of makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
