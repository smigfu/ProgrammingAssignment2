## Matrix inversion can be a costly operation, especially if the same matrix is
## inverted over and over again. To avoid computing inverses which already have
## been solved, this file provided two methods:
##
## makeCacheMatrix: Will create a cache matrix which can store a matix as well
##                  as the corresponding inverse.
## cacheSolve: Will check if the input cache matrix has already been solved and  
##             return the cached version if possible. Else it will solve it and 
##             store the result in its internal cache.


# Creates a cache matrix capable of caching its inverse. The returned cache matrix
# will contain four methods: setmatrix, getmatrix, setinverse, and getinverse.
#
# Args:
#   m: The non-inversed matrix (must be a matix)
#
# Returns:
#   The cache matrix initialized to the given input matrix.
makeCacheMatrix <- function(m = matrix()) {
  ## variables
  inverse <- NULL
  
  ## functions
  setmatrix <- function(y) {
    m <<- y
    inverse <<- NULL
  }
  
  getmatrix <- function() m
  
  setinverse <- function(y) inverse <<- y
  
  getinverse <- function() inverse
  
  ## return the list of functions for the cache matrix
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Returns the inverse matrix for the given cache matrix. If the inverse has
# already been computed it will return the previous computed value, else it
# will compute the value and store it. The inverse is calculated with the
# solve function.
#
# Args:
#   m  : A cache matrix created with "makeCacheMatrix"
#   ...: Any additional parameters will be handed into the solve function.
#
# Returns:
#   The inverse matrix for the given input.
cacheSolve <- function(m, ...) {
  
  inverse <- m$getinverse()
  if(is.null(inverse)) { ## check if the inverse has been cached
    
    message("No cached data available...calling solve..")
    
    ## use solve for the inverse and store it in matrix
    org <- m$getmatrix()
    inverse <- solve(org, ...)
    m$setinverse(inverse)
  }
  
  inverse
}
