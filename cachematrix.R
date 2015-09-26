## Below are two functions that are used to create a special object that 
## stores a numeric matrix for which an inverse exists and caches its inverse.

## makeCacheMatrix creates a special "matrix" which is a list of 4 functions:
## set(data) - sets the matix
## get() - returns the matrix
## setinverse(inverse) - sets the inverse of the matrix
## getinverse() - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## set inverse to be null for the "special" matrix constuctor
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve returns the inverse of a matrix x of the special type
## "makeCacheMatrix". If the inverse is already in the cache then this result is 
## returned, but if it is not then the inverse is calculated and saved to the 
## cache and then returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## does the inverse exist already?
  i <- x$getinverse()
  if(!is.null(i)){
    message("Retrieving cached result")
    return(i)
  }
  ## if inverse not in cache then calculate it
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
