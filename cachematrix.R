## This program follows the assignment2 example, I
## I have make a little change to ensure it work to cache inverse of matrix
## by calling the solve() function..
## 
## I will explain how the function work inline

## This function make a special matrix object 
## which will auto cache it inverse
## whenever there is change of the matrix value
## the cache inverse is expire the force to recalculate it inverse again
## 
makeCacheMatrix <- function(x = matrix()) {
  
  # At the begining of the function, we initialize our inverseMatrix (i.e our cache Inverse Matrix) to null 
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


## Write a short comment describing this function

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
