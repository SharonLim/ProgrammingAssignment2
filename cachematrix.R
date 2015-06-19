## makeCacheMatrix creates a list containing a function which
## - set the value of the matrix
## - get the value of the matrix
## - set the value of inverse of the matrix
## - get the value of inverse of the matrix
## Summary :  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  inversematrix <- NULL  #set inversematrix to null
  set <- function(y) {
    x <<- y
   inversematrix <<- NULL
  }
  get <- function() return(x);
  setinverseMat <- function(inv) inversematrix<<- inv;
  getinverseMat <- function() return(inversematrix);
  return(list(set = set, get = get,
       setinverseMat = setinverseMat,    #set the value of the matrix
       getinverseMat = getinverseMat))   #get the value of the matrix
}


## cacheSolve function returns the inverse of the matrix. 
## It first checks if the inverse has already been computed. 
## If so, it gets the result and skips the computation.
## If not, it computes the inverse, sets the value in the cache via setinverseMat function.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverseMat()
  if(!is.null(inv)) 
    {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverseMat(inv)
  return(inv)
  
}
