## These functions cache the inverse of a matrix instead of computing it repeatedly. This is beneficial because it saves time and computing energy

## This "makeCacheMatrix" function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inver <<- inverse
  getinv <- function() inver
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This "cacheSolve" function computes the inverse of the matrix returned by the function above. If the inverse has already been calculated then the function retrieves the inverse from the cache. This function assumes that the matrix does not change and that the matrix assigned is always invertible. 

cacheSolve <- function(x, ...) {
  inver <- x$getinv()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  matrix.data <- x$get()
  inver <- solve(matrix.data, ...)
  x$setinv(inver)
  inver
}
