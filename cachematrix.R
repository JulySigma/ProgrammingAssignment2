makeCacheMatrix <- function(X = matrix()) {
  m <- NULL
  set <- function(y) {
    X <<- y
    m <<- NULL
  }
  get <- function() X
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(X, ...) {
  m <- X$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setsolve(m)
  m
}
