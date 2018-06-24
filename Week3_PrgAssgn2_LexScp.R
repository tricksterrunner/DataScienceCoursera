## R Programming ; Week 3, Programming assignment 2 : Lexical Scoping. 
## Caching the Inverse of a Matrix:
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The makeCacheMatrix function above computes the inverse of the special "matrix". 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}


## If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
## This can be achieved using the cacheSolve function above.
