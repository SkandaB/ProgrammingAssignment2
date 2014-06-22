## Functions to cache matrix inversion.
##
## The functions in this module provide a convenient interface
## to access and cache the inverse of a matrix.  Example usage:
##
##
##   # The first call to cacheSolve will calculate the inverse,
##   inverse1 = cacheSolve(cacheableMatrix)
##
##   # Further calls to cacheSolve will be significantly faster, since
##   # they return the value cached during the previous call.
##   inverse2 = cacheSolve(cacheableMatrix)


## makeCacheMatrix -- creates an object to hold a matrix and (later)
## its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  i  <- NULL
  set  <- function(y){
    x <<- y
    i <<- NULL 
  }
  get  <- function() x
  setinverse  <- function(inverse) i  <<- inverse
  getinverse  <- function() i
  list(set= set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## Computes the inverse. should retrieve the inverse 
## from the cach if matrix has not changed

cacheSolve <- function(x, ...) {
  i  <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data  <- x$get()
  i  <- solve(data, ...)
  x$setinverse(i)
  i
}