## The functions makeCacheMatrix and cacheSolve create a matrix object that can cache its inverse 
## and computes the inverse of the matrix. If the inverse has already been calculated, the inverse
##is retrieved from the cache.

## This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <- y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    print("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
