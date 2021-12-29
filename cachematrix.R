## This generates a matrix and cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <-function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function generates the inverse of the matrix resulting from our function. If this has already been done, the following function can pull from cache;
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if(!is.null(inversed)) {
    message("getting data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInversed(inverse)
  
  inverse
}
