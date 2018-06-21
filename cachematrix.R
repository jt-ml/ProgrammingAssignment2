## 
## a pair of functions that cache the inverse of a matrix
##

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  result <- NULL
  
  set <- function(invertible_matrix) {
    ## assume invertible_matrix is always invertible
    x <<- invertible_matrix
    result <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(inverse) {
    result <<- inverse
  } 
  
  getInverse <- function() {
    result
  } 
  
  ## return 
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  result <- x$getInverse()
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$get()
  
  result <- solve(data, ...)
  
  x$setInverse(result)
  
  ## return
  result
  
}

