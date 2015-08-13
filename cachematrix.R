## The function creates an inverse of a matrix. The result is stored in a 
## separate variable for use later to save time by preventing a duplicate
## calculation.

## This function will cache the a variable storing the inverse of input matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y #x is now a global variable
      m <<- NULL # m is now a global variable
    }
    get <- function() x
    
    setinverse <- function(solve) m <<- inverse 
    getinverse <- function () m  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This takes an input matrix and returns the inverse; however, if the inverse is
## already calculated it will generate the known value inplace of recalculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
