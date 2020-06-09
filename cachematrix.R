## These functions set the inverse of a matrix and hold it
## in a subenvirontmen so if you need get the inverse
## many times in a code, these functions late lesser than if
## you do thats in a process one step by time


## The following function creates a special
## "matrix" object that can cache irs
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}



## And the next computes the inverse of
## special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been
## calculated (and the matrix has not changed)
## then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
