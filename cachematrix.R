## This program follows the assignment2 example, I
## I have make a little change to ensure it work to cache inverse of matrix
## by calling the solve() function..
## 
## I will explain how the function work inline

## This function make a special matrix object 
## which allow the caller to 
## a) get the matrix
## b) set a new matrix, which set cache inverse null, so that it force the caller to re-calculte the inverse  again
## c) get the matrix inverse
## d) set the matrix inverst
## 
makeCacheMatrix <- function(x = matrix()) {
  
  # At the begining of the function, we initialize our inverseMatrix to null
  inverseMatrix <- NULL
  
  # Whener there is change of the matrix, we set the new_matrix to x
  # and reset the inverseMatrix to null
  # this will force the cacheSolve to recompute the inverse again.
  set <- function(new_matrix) {
    x <<- new_matrix
    inverseMatrix <<- NULL
  }
  
  #
  # Return the matrix 
  #
  get <- function() x
  
  # Allow caller to pass in inverse of matrix, and assign to our cache inverseMatrix
  setInversematrix <- function(solveInverseMatrix) inverseMatrix <<- solveInverseMatrix
  
  # Return the cache Inverse Matrix, ie inverseMatrix to caller.
  getInversematrix <- function() inverseMatrix
  
  list(set = set, get = get,
       setInversematrix =  setInversematrix,
       getInversematrix =  getInversematrix)
}


## 
## This function will first to check if a given matrix
## have inverse store in cache 
## if yes, the cache inverse is return to caller
## if not, the function will compute the inverse by calling the solve() function
## and pass the newly compute inverse to matrix object.
## the next call of cacheSolve of the same matrix will return the inverse from cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInversematrix()
  if(!is.null(inverseMatrix)) { 
    message("getting cached data")
    return(inverseMatrix)
  }
  matrix <- x$get()
  inverseMatrix <- solve(matrix, ...)
  x$setInversematrix(inverseMatrix)
  inverseMatrix
}
