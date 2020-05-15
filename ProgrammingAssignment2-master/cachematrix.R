## R Programming Assignment 2: Lexical Scoping
## Assignment: Caching the Inverse of a Matrix

## This function "makeCacheMatrix" creates matrix  that can cache its inverse.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function "cacheSolve" computes the inverse of the matrix created 
## by the previous Function "makeCacheMatrix".


cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)){
    messgae("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
