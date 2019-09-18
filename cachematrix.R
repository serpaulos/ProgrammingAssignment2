##creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inversor <- NULL
  set = function(y) {
  x <<- y
  inversor <<- NULL
  }
  get = function() x
  setinve <- function(inverse) inversor <<- inverse 
  getinve <- function() inversor
  list(set = set, get = get, setinve = setinve, getinve = getinve)
}


## Funcion to storage and process the inverse of a matrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversor = x$getinve()
  
  # if the inverse has already been calculated
  if (!is.null(inversor)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inversor)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inversor = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinve function.
  x$setinve(inversor)
  
  return(inversor)
}
