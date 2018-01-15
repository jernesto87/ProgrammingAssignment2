## Put comments here that give an overall description of what your
## functions do

## The "object" vector where the values calculated will be stored and retrieved as required

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solveM) i <<- solveM
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Based on the example provided in the instructions of this assignment and the same idea of
## validating if there is already a cached copy of the matrix on memory. The "object" 
## vector is used as a repository to check if the value is available (and it is the same
## input matrix) or it would need to be re-calculated

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Retrieving already cached matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
