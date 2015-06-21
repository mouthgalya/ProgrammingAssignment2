##This R file contains a pair of functions that calculate and cache the inverse of a matrix. 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  minverse<-NULL
  set<-function(y){
    x<<-y
    minverse<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) minverse<<- solve
  getmatrix<-function() minverse
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## CacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
