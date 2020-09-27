## two functions that cache the inverse of a matrix

## This function creates a "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y){
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) a <<- solveMatrix
  getInverse <- function() a
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function gives the inverse of "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  data <- x$get()
  a <- solve(data)
  x$setInverse(a)
  a    
}
