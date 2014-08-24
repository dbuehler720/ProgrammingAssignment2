## Matrix object with cacheable inverse

## Makes a list containing functions to get / set values for a matrix and its inverse, assumes matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## retrieves inverse of a makeCacheMatrix matrix; uses cached inverse if it has already been computed once

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setInverse(inv)
  inv
}
