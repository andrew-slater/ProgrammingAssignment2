## These two functions allow storing a matrix with its inverse
## To avoid re-computation

## This function creates a special "matrix" object
## assumed square & invertible
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize inverse
  set <- function(y) { #initialize or overwrite matrix contents
    x <<- y
    inv <<- NULL #clear stored value of inverse since no longer valid
  }
  get <- function() x #return current contents of matrix
  setinv <- function(inverse) inv <<- inverse #store inverse
  getinv <- function() inv #return current stored value of inverse
  #return functions needed to manipulate matrix and stored inverse
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then this function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv() #get currently stored value of inverse
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv) #exit function returning pre-calculated inverse
  }
  #since stored inverse was null, 
  #need to get contents of matrix to calculate
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) #store inverse
  inv #return inverse
}
