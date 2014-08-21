## Coursera Assignment #2 - Caching the Inverse of a Matrix
## R Sangole

# This function accepts an argument of the datatype matrix, in 'mat'
# The function initially sets invr to NULL. This is so that when the cacheSolve
# function is first called, x$getinv() returns a NULL to 'inverse'. This allows
# usage of the if(!is.null(inverse)) formulation in that function to recall a cached
# value.
# Thereafter, 4 functions within the function are defined:
# set: Enables a manual setting of the matrix in 'mat' (Stored in the set function's parent environment
# i.e. makeCacheMatrix function's environment)
# get: Gets the matrix 'mat'
# setinv: Saves the inverse matrix passed to the func to 'invr' (Stored in the setinv function's parent 
# environment, i.e. makeCacheMatrix function's environment)
# getinv: Retrieves the inverse matrix stored in invr

makeCacheMatrix <- function(mat = matrix()) {
  invr <- NULL
  set <- function(y){
    mat <<- y
    invr <<- NULL
    print(invr)
  }
  get <- function(){
    mat
  }
  setinv <- function(inv){
    invr <<- inv
  }
  getinv <- function(){
    invr 
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function accepts a func list as an argument, in 'x'
# It asks for an inverse via getinv(), and stores in 'inverse'
# If 'inverse' is NOT null, the inverse has been calculated before, and gets returned
# If 'inverse' is null, it needs to be calculated and stored
# Inverse is calculated using solve() and gets stored in the makeCacheMatrix environment
# using setinv()
cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached inverse. Not recalculating.")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinv(inverse)
  inverse
}
