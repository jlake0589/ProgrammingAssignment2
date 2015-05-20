## The makeCacheMatrix and cacheSolve functions allow for the creation
## of a matrix object whose inverse can be cached in memory to conserve
## costly computation, in the event that the inverse of a given matrix
## has already been computed and cached

## This function creates a matrix object that can cache its inverse.
## This function creates a list containing functions to set and get
## the value of the matrix as well as set and get the value of the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <-function() x
  
  setInverse <- function(inverse) m <<- inverse
  
  getInverse <-function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the matrix returned by the
## makeCacheMatrix function. If the matrix has not changed and the
## inverse has already been calculated, this function will retrieve 
## the inverse from the cache rather than re-compute the inverse

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  if(!is.null(m)) {
    
    message("getting cached data")
    
    return(m)
  }
  
  data<- x$get()
  
  m <- solve(data, ...)
  
  x$setInverse(m)
  
  m
}
