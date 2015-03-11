## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## m is the cached matrix
  m <- NULL
  ## the holds the original data set, a matrix, in the object
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## get retrieves the stored matrix data
  get <- function() x
  ## setinverse writes the inverse of the matrix object into the cache
  setinverse <- function(i) m <<- i
  ## getinverse retreives the cached inverse matrix
  getinverse <- function() m
  list (set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
