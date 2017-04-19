## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse <- NULL
  set <- function(x) {
    m <<- x;
    inverse <<- NULL;
  }
  get <- function() m
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: computes the inverse of the special "natrix" returned by makeCachematrix function. if the inverse is already been calculated the retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
         inverse <- m$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- m$get()
  invserse <- solve(data, ...)
  m$setinv(inverse)
  inverse
## Return a matrix that is the inverse of 'x'
}
