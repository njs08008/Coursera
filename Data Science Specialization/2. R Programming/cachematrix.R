## These functions work together to calculate the inverse
## of a nonsingular matrix and then store that inverse
## in memory.

## makeCacheMatrix takes a matrix x as input and 
## outputs a list that acts to store data such as the
## the input matrix and its inverse (once that is known).
## This data will be needed when cacheSolve runs.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<-y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes the output of makeCacheMatrix as its
## input and then outputs the inverse of the original
## matrix x. When cacheSolve runs on given input for the
## first time, it computes the inverse of x and then stores
## that data within makeCacheMatrix. All subsequent times
## cacheSolve will instead extract the inverse from
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
