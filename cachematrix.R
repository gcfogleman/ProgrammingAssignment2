## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly.  The two funcitons below allow the value of the
## inverse of a matris to be cached and then later retrieved from the cache.
## It is assumed that the matrix supplied is invertible.
## the R command   "M <- makeCacheMatrix()" creates the object M
## the R command   "M$set(x)"  sets the matrix to x and computes the inverse of x
## the R command   "M$get()"  retrieves the matrix x
## the R command   "M$getI()" retrieves the inverse of x
## the R command   "cacheSolve(M)"  retrieves the inverse of x, if it exists, from the cache


## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Set the default value of the inverse to NULL
     inv <- NULL
  ## The set function sets the matrix and calculates the inverse
     set <- function(y) {
          x <<- y
          inv <<- solve(x)
     }
  ## The get function gets the matrix
     get <- function() x
  ## The getI function gets the inverse of the matrix
     getI <- function() inv
  ## Return a list of the functions set, get, and getI
     list(set = set, get = get,
          getI = getI)  
}



## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## get the inverse using the getI function
     result <- x$getI()
  ## if the matrix has been set, return the cached inverse
     if(!is.null(result)) {
          message("getting cached data")
          return(result)
     }
  ## otherwise return a message that the matrix has not been set
     message("no matrix set")
}
