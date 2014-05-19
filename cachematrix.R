## This code represents a pair of functions that cache the inverse of a matrix
## since matrix inversion is usually a costly computation, it is best to take the inverse rather
## than compute it repeatedly.  This could save time in the long run.

## makeCacheMatrix: function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv.cache <- NULL
  
  set <- function(y) {
    x <<- y
    inv.cache <<- NULL
  }
  get <- function() x
  set.inverse <- function(i) inv.cache <<- i
  get.inverse <- function() inv.cache
  
  list(set = set, 
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse
  
      )
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$get.inverse()   
  
  # if the inverse exists
  if(!is.null(inverse)) {
    
    return(inverse)
    
  } else {
    
    # If not:

    data <- x$get()
    inverse <- solve(data, ...)
    x$set.inverse(inverse)
    
    return(inverse)
    
  }
}
