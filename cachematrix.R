## Put comments here that give an overall description of what your
## functions do
#Functions generally solve for the inverse of a matrix.
#i here is just short for inverse matrix

## Write a short comment describing this function
#This function creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  #First set the matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Get the matrix
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#This function computes the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Check if the inverse has been calculated already
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}