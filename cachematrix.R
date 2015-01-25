## These functions inveter a matrix and cache it to save time when 
## the inversion of the matrix is needed again. 


## Creates a matrix oject that can cache it's own inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
   
}

## Calculates the inverse of the makeCacheMatrix function. If the inverse has 
## already been calculated for the matrix, then cacheSolve will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse() 
  ## If the matrix exists, don't compute it, just retrieve it
  if(!is.null(m)) {
    message("getting cached inverse of the matrix")
    return(m)
  }
  ## If the inverse of the matix doesn't exist, use solve() to invert it 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

  
  
}
