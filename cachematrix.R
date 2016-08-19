## create a function to cache the inverse of a square matrix in order to avoid
## repeated conversions. Check to see if there is a cached inverse matrix before
## performing the inverse computation

## creates and returns a list of functions to be used as input by cacheSolve
## the list contains functions to set the matrix, get the matrix, set the inverse
## of the matrix and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check to see if inverse of matrix has been calculated and if so, return the
## cached value. If it has not been calculated and cached, preform the calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
