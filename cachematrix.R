## makeCacheMatrix(x, ...) sets/gets a square matrix or its inverse into the cache.
## cacheSolve(x,...) either gets the inverse matrix from the cache or calculates the inverse matrix.

## Given a square matrix,
## This function creates 4 functions - set, get, setInverseMatrix and getInverseMatrix
##   set: sets the square matrix into the cache
##   get: gets the square matrix from the cache
##   setInverseMatrix: sets the inverse matrix into the cache
##   getInverseMatrix: gets the inverse matrix from the cache
## Then it returns a list containing the 4 functions. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  getInverseMatrix <- function() m
  list(set = set, get = get, setInverseMatrix=setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Given a square matrix,
## This function:
##   Retrieves the inverse matrix from the cache.
##   If the cache does not contain the inverse matrix, 
##     it calculates the inverse matrix and caches it.
## Then it returns the inverse matrix

cacheSolve <- function(x, ...) {  
  ##Get the inverseMatrix from the cache
  m <- x$getInverseMatrix()
  
  ##If cached
  if (!is.null(m)) {
    message("Getting cached data")
    return(m)           ##Return the cached inverse matrix
  }
  
  ##Else
  data <- x$get()       ##Get the matrix to inverse
  m <- solve(data, ...) ##Calculate the inverse matrix
  x$setInverseMatrix(m) ##Set the inverse matrix into the cache
  m                     ##Return the inverse matrix
}
