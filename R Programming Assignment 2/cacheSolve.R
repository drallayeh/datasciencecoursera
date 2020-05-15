##R Programming Assignment 2: Lexical Scoping
##makeVector Function

makeVector <- function(x = numeric()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}

##Cachmean Function

cachmean <- function(x, ...){
  m <- x$getmean()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

##makeCacheMatrix Function

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

##cacheSolve Function

cacheSolve <- function(x, ...){
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

## Testing Functions

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()