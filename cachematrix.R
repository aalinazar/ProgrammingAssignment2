## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  
  
  # define functions to get the value of the matrix
  get <- function() x
  # define functions to calculate the inverse of the matrix, using solve function
  setSolve  <- function(solve) inv_x <<- solve
  # define functions to get value of the inverse 
  getSolve <- function() inv_x
  
  list(set = set, get = get,
       setSolve  = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # check cache of the inverse matrix
  inv_x <- x$getSolve()
  if(!is.null(inv_x)) {
    message("getting cached data")
    return(inv_x)
  }
  
  # if not cached, then compute the inverse
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setInverse(inv_x)
  inv_x
}
