## The following functions makeCacheMatrix() and cacheSolve() 
## create a square invertible matrix and store the value in the cache environment
## for retrieval.

## makeCacheMatrix creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## store the cache value and initialize it to NULL
  cache <- NULL
  
  ## create the matrix
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## invert the matrix and store it in the cache
  setMatrix <- function(inverse) cache <<- inverse 
  
  ## get the inverted matrix from the cache
  getInverse <- function() cache
  
  ## return the functions
  list(set = set, get = get,
       setMatrix = setMatrix,
       getInverse = getInverse)
}


## cacheSolve computes the inverse of the special "matrix" object 
## returned by the function makeCacheMatrix()
## If the inverse has already been calculated and the matrix has not changed, 
## the function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## get the value of the cached matrix
  cache <- x$getInverse()
  
  ## verify the value of the cached matrix is not NULL
  if (!is.null(cache)) {
    ## if it exists, return message and the value of the cached matrix
    message("getting cached matrix")
    return(cache)
  }
  
  ## if the value is NULL (does not exist), create a matrix
  matrix <- x$get()
  ## assuming the matrix is a square invertible matrix, 
  ##use the solve() function to get its inverse
  cache <- solve(matrix, ...)
  ## set the inverted matrix 
  x$setMatrix(cache)
  ## return the value of the matrix
  return(cache)
}
