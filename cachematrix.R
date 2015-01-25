## This R script consist of a pair of functions that cache the inverse of a matrix and serves as assignment 2 for the R Programming Coursera course.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
  n <- NULL
  set <- function(y) {
    m <<- y
    n <<- NULL
  }
  get <- function() m
  setInverse <- function(solve) n <<- solve
  getInverse <- function() n
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setInverse(n)
  n
}