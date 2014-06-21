## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x=matrix()) {
 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }                                               ## set matrix
  get <- function() x                             ## get matrix
  setInverse <- function(inverse) m <<-inverse    ## set inverse matrix
  getInverse <- function() m                      ## get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}                                                ## Creates a list of functions that can cache the inverse of a matrix.

cacheSolve <- function(x, ...) {

  m <- x$getInverse()                            ## get the x matrix's inverse cache
  if ( ! is.null(m)) {                           ## if the cache of inverse maxtrix has been previously calculated
    message("getting cached data")               ## display the message "getting cached data"
    return(m)                                    ## return the previously cached inverse matrix
  }
  m <- solve(x$get())                            ## if the inverse matrix is not cached, get the matrix, calculate the inverse of the matrix 
  x$setInverse(m)                                ## store the inverse matrix in cache using the set function of makeCacheMatrix
  m                                              ## return the inverse matrix  
}                     

