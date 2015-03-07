## Data Science - R Programming - Programming Assignment 2

## Create a matrix suitable for caching the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # inverse of matrix at initialization
  inv <- NULL
  
  # setter, called at initialization: store original matrix in x, inverse is NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter, return original matrix
  get <- function() x
  
  # set inverse of matrix in cache
  setinverse <- function(inverse_matrix) inv <<- inverse_matrix
  
  # get inverse from cache; if not yet calculated, inverse will be NULL
  getinverse <- function() inv
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculate inverse of matrix, use cached version if present
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  # check if there is valid (not NULL) cached file
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # otherwise,get original data
  data <- x$get()
  
  # compute inverse
  inv <- solve(data, ...)
  
  # store in cache
  x$setinverse(inv)
  
  # return inverse
  inv
}