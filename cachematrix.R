## Caching the Inverse of a Matrix
## Below functions create a special object that stores a matrix and caches its inverse

## Below function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) inve <<- inverse
  getInverse <- function() inve 
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse
  )
  
}


## This function computes the inverse of the special "matrix" created by makeCacheMatrix above
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inve <- x$getInverse()
  if(!is.null(inve)){
    message("getting cached data")
    return(inve)
  }
  mat <- x$get()
  inve <- solve(mat,...)
  x$setInverse(inve)
  inve
}