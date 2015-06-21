##This R file contains a pair of functions that calculate and cache the inverse of a matrix. 

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
##It takes an input matrix and returns a list of 4 functions used to get/set the input matrix and its inverse

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
## It computes the inverse of the matrix only if the inverse is not yet available in the cache
## and the input matix has not yet changed.

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
