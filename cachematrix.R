## At last, you can cache your inverted matrices, like a squirrel hiding delicious numerical acorns!
## * Use makeCacheMatrix(matrix) to create a cache object for your matrix
## * Pass that object to cacheSolve(cacheMatrix) to check the cache for the inverse, if it exists, 
##   or calculate the inverse and cache it away for the future

## makeCacheMatrix takes your numerical matrix and then returns a special caching matrix with functions to:
# - set the value of your matrix
# - get the value of your matrix
# - set the inverse of your matrix
# - get the inverse of your matrix
## This function does not actually calculate the inverse of your matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverseMatrix) m <<- inverseMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes your cacheMatrix, created above, and will check to see if the inverse is already cached
## - if it's cached, cacheSolve warns "getting cached data" and returns the cache
## - if it's not cached, cacheSolve calculates the inverse of your original matrix, caches it, 
##   and returns the value
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data, ...)
  x$setinverse(m)
  m
}

