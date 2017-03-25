##Two functions that can be used to save time when solving for the inverse of
##a matrix in R. These two functions cache the inverse rather than compute it
##repeatedly.

# Function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmx <- NULL
  set <- function(y) {
    x <<- y
    invmx <<- NULL
  }
  get <- function()
    x
  setinvmx <- function(inverse)
    invmx <<- inverse
  getinvmx <- function()
    invmx
  list(
    set = set,
    get = get,
    setinvmx = setinvmx,
    getinvmx = getinvmx
  )
}

#Function computes the inverse of the special "matrix" returned by the
#makeCacheMatrix function. If the inverse has already been calculated
#then retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmx <- x$getinvmx()
  if (!is.null(invmx)) {
    message("getting cached data")
    return(invmx)
  }
  m.data <- x$get()
  invmx <- solve(m.data, ...)
  x$setinvmx(invmx)
  invmx
}
