## The following methods reduce the cost of matrix inversion by caching the inverse 
## of a matrix rather than computing it repeatedly. The pair of functions  cache the inverse 
## of a matrix

## makeCacheMatric creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSOlve computes the inverse of the special "matrix" returned by makeCachematrix above. If the 
## inverse has already been calculated (and the matrix has not changed), then the function retrieves the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
