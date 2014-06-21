## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }                                            # set matrix
  get<-function() x                            # get matrix
  setmatrix<-function(solve) m<<- solve        # set inverse matrix
  getmatrix<-function() m                      # get inverse matrix
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)                    #create a list
}

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()                            #get the x matrix's inverse cache
  if(!is.null(m)){                            #if the cache of inverse maxtrix has been previously calculated
    message("getting cached data")            #display the message "getting cached data"
    return(m)                                 # return the previously cached inverse matrix
  }
  matrix<-x$get()                           # if the inverse matrix is not cached, get the matrix  
  m<-solve(matrix, ...)                     # calculate the inverse of the matrix
  x$setmatrix(m)                            # store the inverse matrix in cache using the set function of makeCacheMatrix
  m                                         # return the inverse matrix
}
