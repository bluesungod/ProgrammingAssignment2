## R Programming Assignment 2: Caching Inverse of a Matrix

## These functions will try to speed up the process of
# getting the inverse of a matrix.

## First we will create a special object that we can use
# to cache an inverse. Then in another function, we will
# compute the inverse of the special object "matrix" after
# first checking to see if it is already in the cache.


## To start, let's create the special object, which will
# help us by caching the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    ## The <<- operator assigns a value outside the given
    # environment.
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  ## This code returns the "special object" to the console,
  # which is a list that contains functions. We can use the
  # functions contained as tools to help us calculate the
  # inverse of the matrix.
}


## The second function, defined below, will actually compute
# the value of the matrix's inverse, after first making sure 
# the inverse hasn't already been calculated and stored in 
# the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  if(!is.null(inv)) { # If cache for inverse isn't empty...
    
    message("getting cached data")
    return(inv) # ...get the cached value. No work needed!
  }
  
  ## If cache IS empty, then the code below does the job,
  # using the functions in the special object we created.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv) # Stores result in cache for future use.
  
  ## Finally, we return the value for the inverse, and we
  # are all set!
  inv
}

## Good work!