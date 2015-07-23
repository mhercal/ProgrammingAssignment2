## This script sets a matrix, calculates its inverse, caches its
## and checks if the inverse has already been calculated.
## If so it will retrieved the result from cache.

## MakeCacheMatrix sets a matrix (we assume it is square and invertible)
##Gets the matrix, sets the inverse and gets the inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##cacheSolve calculates de inverse of the matrix returned by MakeCacheMatrix
##If it is already been calculated the inverse is retrieved from Cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
