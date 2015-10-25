## Put comments here that give an overall description of what your
## functions do

## Script to fulfill the following assignment requirements:

## Write the following functions:

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## I started with the examples given in the assignment and modified to generate the
## inverse of a matrix and cache it.

makeCacheMatrix <- function(x = matrix()) 
  {
  ##Make the matrix inverse value null to initializw
    c <- NULL
    
  set <- function(y)
      {x <<- y
      c <<- NULL}
  
  ## gets the value
  get <- function() x
  ## calculates the inverse
  setsolve <- function(solve) c <<- solve
  ## gets the inverse
  getsolve <- function() c
  
  ##pass the value out
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
} 


## Check to see if the inverse has already been solved for, if so, use cache
## If it hasn't been solved, solve it

cacheSolve <- function(x, ...) 

  ## Return an inverse matrix of 'x'
  {
  c <- x$getsolve()
  if(!is.null(c)) {
    message("getting inverse of the matrix from cache")
    return(c)
  }
  
  ##If it isn't there, go solve for it
  
  data <- x$get()
  c <- solve(data, ...)
  x$setsolve(c)
  c
}

