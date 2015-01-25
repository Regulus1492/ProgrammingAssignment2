## makeCacheMatrix creates a matrix, and then caches the inverse of the matrix
##CacheSolve uses makeCacheMatrix and calculates the inverse of the matrix, using the cache 
##and check if there are calculations that are not needed to be redone, i.e. if the matrix does not change. 

##FUNCTION
##Matrix
##Cache Matrix
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

##Cache Solve


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

##Example
#x <- matrix(rnorm(200), 10, 10)
#matrix_x<-makeCacheMatrix(x)
#inv<-cacheSolve(matrix_x)