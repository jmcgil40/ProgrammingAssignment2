## This pair of functions will cache the inverse of a matrix such 
## that it can be recalled after it has already been calculated
## once. 

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above or retrieves the inverse from the cache 
## if the inverse has already been calculated.
cacheSolve <- function(x, ...) {
        
  m<-x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinverse(m)
  
  ## Return a matrix that is the inverse of 'x'
  m
}
