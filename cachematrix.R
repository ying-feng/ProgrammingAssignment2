## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creats a special "vector" containing a list of functions
## to 1) set the value of the matrix; 2) get the value of the matrix; 
## 3) set/store the inverse of the matrix; 4)get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_new) inverse_x <<- inverse_new
  getinverse <- function() inverse_x
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Write a short comment describing this function
## This function calucalates the inverse of the matrix created
## with makeCacheMatrix function. If the inverse has
## already been calculated (and the matrix has not changed), then
## the function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
