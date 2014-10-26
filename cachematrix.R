## >We will create a special object that stores a matrix
## and caches its inverse<

## this function below (makeCacheMatrix):
## creates a special matrix object that can cache its inverse.
## by doing the following:

## 1.set the value of the input matrix.
## 2.get the value of the input matrix.
## 3.set the value of the matrix inverse.
## 4.get the value of the matrix inverse.


makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function below (cacheSolve):
## computes inverse of the special matrix returned by 'makeCacheMatrix'.
## If the inverse is already calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
