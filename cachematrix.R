## The makeCacheMatrix and cacheSolve functions allow for the creation
## of a matrix object whose inverse can be cached in memory to conserve
## costly computation, in the event that the inverse of a given matrix
## has already been computed and cached

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <-function() x
  setMean <- function(mean) m <<- mean
  getMean <-function() m
  list(set = set, ger = get,
       setmean = setmean,
       getmean = getmean)
}


## This function computes the inverse of the matrix returned by the
## makeCacheMatrix function. If the matrix has not changed and the
## inverse has already been calculated, this function will retrieve 
## the inverse from the cache rather than re-compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
