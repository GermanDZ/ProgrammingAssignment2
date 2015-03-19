## makeCacheMatrix function take a matrix and returns a list wich contains the
## matrix and provides functions to cache the inverse.
##
## cacheSolve function take a "cacheMatrix", returned by makeCacheMatrix
## function, and return the inverse of the original matrix. This function reuse
##the same result from a previous execution to avoid extra computing.
##
##
## How to use:
##
## 1) define a matrix, for example:
##   simpleMatrix <- matrix(c(2,8,4,8), nrow=2)
##
## 2) make the cacheableMatrix:
##   cacheableMatrix <- makeCacheMatrix(simpleMatrix)
##
## 3) solve the inverse:
##   invertedMatrix <- cacheSolve(cacheableMatrix)
##
## The inverted matrix is:
##      [,1]   [,2]
## [1,] -0.5  0.250
## [2,]  0.5 -0.125
##
## repeating the 3rd step will not perform extra computing, just return the
## cached result.


## This function takes a matrix as input and return a list with 4 functions
## and 2 scoped variables in the list's environment which stores the input
## matrix and the calculated result.

makeCacheMatrix <- function(input = matrix()) {
  inverse <- NULL
  set <- function(y) {
    input <<- y
    inverse <<- NULL
  }
  get <- function() input
  setInverse <- function(result) inverse <<- result
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function take a special list returned by makeCacheMatrix and return
## their inverse.
## To avoid extra computation, this function checks if the list holds a cached
## result.
## When the inverted matrix was never calculated, this function make solve the
## matrix and store the result in the cache (a variable inside the list's
## environment).

cacheSolve <- function(x, ...) {
  result <- x$getInverse()
  if(!is.null(result)) {
    return(result)
  }
  matrixData <- x$get()
  result <- solve(matrixData, ...)
  x$setInverse(result)
  result
}
