## Caching the Inverse of a Matrix:-
## Matrix inversion is costly computation. 
## Sometimes its beneficial to caching the inverse of a matrix rather than compute it repeatedly.
## following given pair of functions are used to create a special object that stores a matrix and caches its inverse.
## using following function creates a special matrix object that cache the inverse.

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


## Now computing the inverse of the special "matrix" created by makeCacheMatrix. 


cacheSolve <- function(x, ...) {
  ## matrix of the inverse of 'x'
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
#testing
my_matrix$getInverse()
cacheSolve(my_matrix)
