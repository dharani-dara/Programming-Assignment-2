##This function contains the functions that are called when we need to cache a result
makeCacheMatrix <- function(x = matrix()) {
m<- NULL
set<-function(y){
  x<<-y
  m<-NULL
}
get<-function()x
setInverse<-function(inverse)m<<-inverse 
getInverse<-function()m
list(set=set,get=get,
     setInverse=setInverse,
     getInverse=getInverse)
}


## The following function gets the inverse of the matrix, but it first checks it it has already been calculated
## If so, it gets the inverse from the cache and therefore skips the computation. Otherwise, it calculates the inverse and sets the result
## in the cache with the SetInverse function 

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <-x$get()
  m<-solve(matrix,...)
  x$setInverse(m)
  m
}
