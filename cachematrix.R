#This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # inverted matrix cache storage
  inverted_x <- NULL
  
  # set original matrix
  setMatrix <- function(y) {
    x <<- y
    # when we set new data, we will clear cache
    inverted_x <<- NULL
  }
  
  # get original
  getMatrix <- function() {
    x
  }
  
  # set inverted matrix to cache
  setInverted <- function(inverted) {
    inverted_x <<- inverted
  }
  
  # get inverted matrix from cache
  getInverted <- function() {
    inverted_x
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverted = setInverted, getInverted = getInverted)
}

#This function computes the inverse of the special matrix returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverted_x <- x$getInverted()
  if (!is.null(inverted_x)) {
    return(inverted_x)
  }
  
  inverted_x <- solve(x$getMatrix(), ...)
  x$setInverted(inverted_x)
  return(inverted_x)
}
