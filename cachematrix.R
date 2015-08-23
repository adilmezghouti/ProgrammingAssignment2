## Functions allowing you to create a special matrix that has caching capabilities.

## This function creates a matrix and allows you to read it, set it and calcualte the mean of it
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL    
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,get = get,getinverse = getinverse,setinverse = setinverse)
}


## Calculates the inverse of a matrix and caches the result for future direct use
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
