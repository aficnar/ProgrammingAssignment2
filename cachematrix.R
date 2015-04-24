## The idea here is to create a matrix object M in which we can store some matrix, and
## calculate its inverse. If the inverse has been already calculated, it should be
## simply retrieved, without repeating the inversion.

## M <- makeCacheMatrix() will assign to M a list of functions, and we can set the 
## value of the matrix by M$set(...) where ... is some matrix (this can be retrieved 
## by M$get()). Then cacheSolve(M) will calculate the inverse of the matrix, and set 
## it "globally", so that the next time we ask for the inverse, it will retrieve 
## it from the global environment.

# Sets a special, globally defined matrix object
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) { # Set the value of matrix x
    x <<- y
    inverse <<- NULL
  }
  get <- function() x # Get the value of matrix x
  setInv <- function(inv) inverse <<- inv # Set the inverse of the matrix globally
  getInv <- function() inverse # Retrieve the inverse of the matrix
  list(set = set, get = get,
       setInv = setInv, getInv = getInv) # Output the list of all these functions
}

# Calculates the inverse of the matrix in the above defined matrix object
cacheSolve <- function(x, ...) {
  inverse <- x$getInv() # Get the inverse of the matrix
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse) # Check if the inverse had been already calculated and, if so, return the cached value
  }
  data <- x$get() 
  inverse <- solve(data, ...) # If it hasn't been calculated earlier, calculate it now
  x$setInv(inverse) # Store it in the "globally" defined matrix object
  inverse
}