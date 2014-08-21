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
  invr <- NULL #invr set to NULL every first time makeCacheMatrix is run
  set <- function(y){
    mat <<- y #Sets 'mat' in makeCacheMatrix func's environment to y
    invr <<- NULL #Sets 'invr' in makeCacheMatrix func's envir to NULL, since a new matrix is assigned to mat
  }
  get <- function(){
    mat
  }
  setinv <- function(inv){
    invr <<- inv #sets 'invr' in makeCacheMatrix func's envir to the inverse obtained thru argument 'inv' 
  }
  getinv <- function(){
    invr 
  }
  list(set = set, get = get, #function list
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
  inverse <- x$getinv() #get existing inverse, possibly NULL
  if(!is.null(inverse)) { #if NOT NULL, run this...
    message("Getting cached inverse. Not recalculating.")
    return(inverse) #return exits the function completely
  }
  #if NULL, run this...
  mat <- x$get() #get the matrix
  inverse <- solve(mat, ...) #calculate the inverse
  x$setinv(inverse) #store in makeCacheMatrix func's environment
  inverse #return inverse
}
