

## takes the matrix as input and sets the initial value of the inverse as null

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## checks what the stored value of the niverse is, in case it's null caluclates the inverse and stores it to the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}