## The 2 functions below, together, enables the user to find the inverse of a matrix, while making
## use of the <<- operator to cache the already computed inverse of a matrix

## The function makeCacheMatrix() accepts a matrix and returns a list of 4 functions that
## enables the user to cache the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) 
{
  
  InvertedMatrix <- NULL  ##Initialise the inverse to NULL
  
  set <- function(y = matrix()) ##Take the arguement of class Matrix 
  {
    m <<- y          ## Set m to a fresh matrix
    InvertedMatrix <<- NULL ## If the matrix is refreshed, the Inverse should be NULLed as it's no longer valid.
  }
  
  ## get(): Simply return the matrix
  get <- function() m 
  
  ## setInverse(): Set the inverse
  setInverse <- function(Inv = matrix()) InvertedMatrix <<- Inv
  
  ## getInverse(): Return the Inverse
  getInverse <- function() InvertedMatrix
  
  ## Return the list of functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function cacheSolve returns:
## 1. The cached Inverse of matrix, if it is already there.
## 2. Otherwise, it computes the Inverse, caches the inverse, and return it.

cacheSolve <- function(x, ...) 
{
  ## Get the Inverse of x 
  Inverse <- x$getInverse()
  
  if(!is.null(Inverse)) ### If Inverse is already cached, return it without having to calculate it.
  {
    message("Getting the cached Inverse")
    return(Inverse) 
  }
  
  ## Otherwise, get the cntents of Matix and compute it Inverse.
  data <- x$get()
  
  Inverse <- solve(data)  ## Compute the inverse using solve()
  
  x$setInverse(Inverse) ## Before returning, cache the inverse.
  
  Inverse  
}
