## This function can create a special matrix, which is a list containing a function to
## do: set and get the matrix, set and get the inverse of the matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setivm <- function(ivm) im <<- ivm
  getivm <- function() im
  list(set = set, get = get,
       setivm = setivm,
       getivm = getivm)
  
}


## cacheSolve function can calculate a inversed matrix created by the above function
## fisrt check if the inversion is already done
## yes, return the cached inversed matrix
## no, calcualte the inversed matrix, set it in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ivm <- x$getivm()
  if (!is.null(ivm)){
    return (ivm)
  }
  ob <- x$get()
  ivm <- solve(ob, ...)
  x$setivm(ivm)
  ivm
}