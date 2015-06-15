## The two functions below create a special object that stores a matrix and
## caches/returns its inverse.  They assume that the matrix is invertible.


## This function returns a list of 4 functions that correspond to the input matrix x
## set() sets the value of the matrix
## get() gets the value of the matrix
## setinv() sets the value of the inverse
## getinv() gets the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes the special object created by makeCacheMatrix() and returns
## either the cached inverse of x or, in its absence, calculates the inverse and
## returns it
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinv(i)
  i
}