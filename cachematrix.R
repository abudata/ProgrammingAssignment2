## The makeCacheMatrix is creating a matrix object that can cache its inverse.            
## The cacheSovle function is calculating the inverse of the above matrix and it checks if 
## the inverse has already been calculted or not. If the inverse is 
## already calculated, then the CacheSolve function is calling the inverse from the Cache and thus, there is no need
## for re-calculate/compute the inverse of the matrix.

## The MakeCacheMatrix function is setting the value of the inv to NULL to provides 
## a defulat value if cacheSolve has not yet been used. Then it sets the value of the
## Matrix as a function y and then chache the value of x to y.
## ...
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
}
get<-function() x
setmatrix<-function(solve) inv<<- solve
getmatrix<-function() inv
list(set=set, get=get,
     setmatrix=setmatrix,
     getmatrix=getmatrix)
}




cacheSolve <- function(x, ...) {
  inv<-x$getmatrix()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setmatrix(inv)
  inv  ## Return a matrix that is the inverse of 'x'
}
