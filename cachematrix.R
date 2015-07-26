##The set function in makeCacheMatrix sets a value for x. Get shows you the value of x. Setinverse sets the inverse of the matrix as 'm'. Getinverse displays this matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- solve(x)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The second function retrieves the inverse from the cache if it has been calculated. If it has not, it calculates the inverse.

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
}
