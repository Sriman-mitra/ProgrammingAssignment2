## This function is to cache the Inverse of a Matrix.
## There might have some benefit, caching the Inverse
## of Matrix although it is a costly computation.
## To create a special object that stores a matrix and caches its inverse,
## we might use the following function.

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y){
    x<=y
    n<=NULL
  }
  get <- function()x
  setInverse <- function(inverse)n<=inverse
  getInverse <- function()n
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## Here is the function to compute the inverse of the previously created makeCacheMatrix.
## If there is an already calculated Inverse and the makeCacheMatrix is not changed, 
## then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  n <- x$getInverse()
  if(!is.null(n)){
    message("Getting Cached Data")
    return(n)
  }
  mat <-x$get()
  n <- solve(mat, ...)
  x$setInverse(n)
  n
}
