## makeCacheMatrix returns a list of functions when it is called 
## passing a matrix to the function.

## The list thus returned has functions that retain access to its data
## thanks to lexical scoping.

makeCacheMatrix <- function(x = matrix()) {
  
  ## 'i' stores inverse
  
  i <- NULL
  set <- function(y) {
    ## <<- used to set the variable in a parent environment 
    x <<- y
    i <<- NULL
  }
  get <- function() x
  ## called to set the variable 'i'
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  ## returns a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSOlve accepts an object returned by a prior call to makeCacheMatrix
## 
## it caches the value in the 'i' variable in the local environment 
## of the list, via the setinv() function

## assumes only invertible matrices are passed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  ## the single parameter option of solve is to return inverse.
  i <- solve(data,...)
  x$setinv(i)
  i
}
