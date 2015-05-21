## The makeCacheMatrix and cacheSolve functions allow for the result of
## of a matrix's inverse to be cached in memory to conserve
## costly computation, in the event that the inverse of a given matrix
## has already been computed and cached

## makeCacheMatrix function creates a list containing four functions to set and get
## the value of the matrix and set and get the value of the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

    # assign m variable to value of null
    m <- NULL
    
    # define set function to set value of matrix when called
    # when matrix is set, m value set to null
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # define get function which returns the current matrix x
    get <-function() x
    
    # define setInverse function which sets the inverse m
    setInverse <- function(inverse) m <<- inverse
    
    # define getInverse function which returns the inverse m
    getInverse <-function() m
    
    # return a list containing the four above functions so
    # they are available to be called
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve function computes the inverse of the matrix in question.
## If the matrix has not changed and the inverse has already been calculated, 
## this function will retrieve the inverse from the cache and return rather 
## than re-compute the inverse. If the inverse has not yet been calculated for
## the matrix, then the inverse is computed, stored in cache and then returned

cacheSolve <- function(x, ...) {
    
    # use getInverse function to assign variable m the value of m stored in 
    # makeCacheMatrix function
    m <- x$getInverse()
    
    # if m is not null, it has been computed already
    # if so, return m and print message so user knows the value is being returned
    # from cache and not being re-computed and function finished
    # otherwise, if statement is skipped and function proceeds
    if(!is.null(m)) {
    
        message("getting cached data")
    
        return(m)
    }
    
    # assign variable matrix the value of the current matrix x in makeCacheMatrix function
    matrix <- x$get()
    
    # assign m the value of the inverse of the matrix using solve function
    m <- solve(matrix, ...)
    
    # call the setInverse function to cache the value of the inverse in variable m of makeCacheMatrix function 
    x$setInverse(m)
    
    # return newly computed and now cached value of m to cacheSolve
    m

}
