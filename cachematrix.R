## The following set offunctions will enable to cache potentially time-consuming inverse matrix computations. 
## By using a cached inverse funtion of a matrix, the inverse matrix computation is optimized.
## If the matrix value has not changed since the last inverse computation, a cahced inverse will be returned.
## The functions take advantage of R's lexial scoping rules

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<-  solve
  getInverse <- function() m
  list(set = set, get= get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the function 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m  
}
