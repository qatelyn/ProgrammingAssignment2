## makeCacheMatrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL 
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) m <<- solve
  
  getInverse <- function() m
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Check if inverse has already been calculated:
  
  m<-x$getInverse()
  
  ## If the inverse has already been caculated, return cached data: 
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  ## If the inverse has not been caculated, use solve function to calculate inverse:  
  
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setInverse(m)
  m
}
