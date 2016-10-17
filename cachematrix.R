## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. These pair of functions can cache the inverse of a matrix.
##
##  - makeCacheMatrix: This function creates a special "matrix" object that can
##                     cache its inverse.
##  - cacheSolve: This function computes the inverse of the special "matrix"
##                returned by makeCacheMatrix above. If the inverse has already
##                been calculated (and the matrix has not changed), then
##                cacheSolve should retrieve the inverse from the cache.


## Contains a matrix that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(v) {
    mat <<- v
    inv <<- NULL  # Initializes inv when matrix changes
  }
  setinv <- function(inverse) inv <<- inverse
  get <- function() mat
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates inverse matrix or retrieves value from cache if it exists
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) { # Inverse matrix exists, returning value
    message("getting cached data")
    return(inv)
  }
  # Inverse matrix does not exists, computing and caching value
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv ## Return a matrix that is the inverse of 'x'
}

