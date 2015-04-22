## This is a pair of functions that allow the user to cache the inverse of a matrix after calculating it.
### This is cool because it can save processing time when handling large matricies.

## makeCacheMatrix()
## Creates a "cacheable matrix" of four functions that stores the value of a matrix and its inverse.
## Arguments: 
### (1) "x", a matrix
### NOTE: "x" MUST be square and invertible
## Output:
### (1) a list of four functions, which stores the value of "x" and its inverse "i" (or a null, if not yet calculated)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function() i <<- solve(x)
  
  getinverse <- function() i
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve()
## Determines if the inverse has already been calculated, and if not, calculates it.
## Arguments:
### (1) A "cacheable matrix," x (a list of four functions as outputted by makeCacheMatrix())
## Output:
### (1) (opt) A message indicating that the inverse was already calculated
### (2) The inversion of x

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  this_matrix <- x$get()
  i <- solve(this_matrix)
  x$setinverse()
  i
}