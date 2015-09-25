## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes in a matrix
## and creates a special list of functions that 
## setMatrix - takes in a matrix and puts it in x
## getMatrix - returns the current matrix stored in this function
## setInv - stores the inverse of x into m variable
## getInv - returns the inverse of x stored in m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # setMatrix is not used but may be useful in the future
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(
    setMatrix = setMatrix, 
    getMatrix = getMatrix,
    setInv = setInv,
    getInv = getInv
  )
}


## Write a short comment describing this function
## cacheSolve takes in a 'makeCacheMatrix' object
## checks to see if this object has already calculated its inverse and uses it
## if not it does the calculation and stores it in the object and displays the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMatrix()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
