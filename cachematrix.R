## This code contains two functions – makeCacheMatrix and cacheSolve – 
#which are intended to accelerate potentially time-consuming computations
#associated with finding the inverse of a matrix. Since inverting 
#a matrix can be time-consuming, where the contents of a matrix are 
#not changing, it may make sense to cache the inverse. When we need 
#it again, it can be looked up in the cache rather than recomputed. 

## "makeCacheMatrix" - The first function - makeCacheMatrix - creates a
#list containing a function to a) create the matrix;
#b) get the the matrix; c) set the inverse of the matrix;
#d) get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## "cacheSolve" - The second function – cacheSolve - 
#calculates the inverse of the matrix created in 
#makeCacheMatrix. It first checks to see if the inverse
#has already been calculated. If so, it gets the inverse 
#from the cache and skips the computation. Otherwise, 
#it calculates the inverse of the matrix and sets the 
#value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}







