## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function resets the variables x and inv.
##It passes descriptions of what should be printed when 
##x$get, x$setinv and x$getinv are executed
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##
## Same here, changed "mean" to "solve" and "m" to "s"
## Write a short comment describing this function
##If Inv is not null, then the value that is in Inv will be used
##If null, caclulations need to be run (as per the functions that have been 
##passed to x$get and x$inverse in the MakeCachematrix function
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting inversed matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
