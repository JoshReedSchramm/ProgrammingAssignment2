## The purpose of this file is to cache a matrix inversion or
## recaulate that inversion on demand if needed

## makeCacheMatrix is a kind of "class" in that it has 4 functions that 
## save both an original and an inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cache solve will take in a matrix and either calculate the inverse of that matrix, or 
## return a previously caculated matrix inversion

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i      
}
