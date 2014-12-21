## makeCacheMatrix(x) - create new object, set result matrix to NULL
## makeCacheMatrix$set(x) - set result matrix to NULL, set source matrix to x
## makeCacheMatrix$get() - return source matrix
## makeCacheMatrix$setsolve(x) - set result matrix to x
## makeCacheMatrix$get() - return result matrix
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


## cacheSolve(x) - if result matrix already been calculated, then return it (with message "getting cached data")
## Otherwise, solve matrix from x, cache the result, and return it
cacheSolve <- function(x) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
