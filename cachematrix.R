## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ##Original matrix
  data <- x$get()
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  ## if inverse matrix is not null and original matrix hasn't changed
  if(!is.null(m) && round(mean(diag(m %*% data)))==1) {
    message("getting cached data")
    return(m)
  }
  
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

