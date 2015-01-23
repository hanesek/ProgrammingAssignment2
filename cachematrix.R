## The assignment consists of 2 functions: the first (makeCacheMatrix) to create a matrix object that can cache
## its inverse and the second (cacheSolve) to compute the inverse of the matrix returned by the first.  If the
## inverse has already been computed cache solve will retrieve the matrix inverse from the cache.

## This function creates a special matrix, which is a list of various functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y){  ## set the value of the matrix and create the cache
    x <<- y  
    m <<- NULL  
  }
  get <- function() x  ## get the value of the matrix
  setinverse <- function(solve) m <<- solve  ## set the inverse of the matrix
  getinverse <- function() m  ## get the inverse of the matrix
  list (set = set, get = get,  ## create list to store the functions
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function calculates the inverse of the matrix, unless it has already been done, then it retrieves the result

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()  ## check if inverse has been calculated
  if(!is.null(m)){   
    message("getting cached data")
    return(m)  ## return the matrix inverse
  }
  data <- x$get()  ## otherwise
  m <- solve(data, ...)  ## caqlculate the matrix inverse
  x$setinverse(m)  ## set the value of the inverse in the cache
  m  ## return the matrix inverse     
}
