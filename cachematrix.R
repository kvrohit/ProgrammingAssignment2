## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions implement caching the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## This matrix object contains four functions
  ## set, get, setInverse and getInverse, fairly simple and self documenting
  ## i.e. set and get is used to set or get the original matrix's value
  ## setInverse and getInverse is used to set or get the inverse matrix's value
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inv) {
    inverse <<- inv
  }
  getInverse <- function() {
    inverse
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If inverse is already calculated, this returns the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Try to get the inverse from cache, if found, return the inverse and we are done
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting matrix inverse from cache")
    return(inverse)
  }
  ## If not found, then
  ## 1. Get the original matrix
  mat <- x$get()
  ## 2. Find the inverse of the matrix
  inverse <- solve(mat, ...)
  ## 3. Save this into the cache
  x$setInverse(inverse)
  ## 4. Return the inverse matrix
  inverse
}
