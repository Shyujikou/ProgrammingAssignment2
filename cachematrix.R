## These two functions solve the inverse of a matrix and cache the result

## makeCacheMatrix takes a matrix as a parameter, and returns
## a special list containing 2 pairs of get and set functions:
## "set" and "get" set or return the matrix itself, while
## "setsolve" and "getsolve" set or return the inverse of the matrix
## (which functions as a cached result)
## If the matrix itself is reset, the cached result will be cleared

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve takes a list returned by makeCacheMatrix(), and calculates
## the inverse of the matrix contained in the list.
## If there exists a cached result, then the cached result will be returned
## otherwise it calls the solve() function to calculate the inverse
## and then calls x$setsolve() to store the result into cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
