# Caching Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  get <- function(){
    x
  }
  set <- function(v) {
    x <<- v
    inv <<- NULL
  }
  
  getinverse <- function() {
    inv
  }
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  #lists the functions for matrix
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


# Returns:
#   The inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  mx <- x$get()
  inv <- solve(mx, ...)
  x$setinverse(inv)
  return(inv)
}