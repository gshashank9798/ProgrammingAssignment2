## makeCacheMatrix function creates a special "matrix" object(which is actually a list)
## It contains four basic functions which are-
## getMatrix : returns the matrix
## setMatrix : sets the matrix
## getMatInv : returns the Inverse of the matrix
## setMatInv : sets the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function takes a special "Matrix"(which is actually a list) as an argument
## At first, It checks if the inverse exists in the cache or not
## If it exists, then it fetches the inverse from the cache
## If not, then it calculates the inverse of the matrix & stores it in the cache


cacheSolve <- function(x, ...) {  
 
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data...")
    return(i)
  }
  else {
    message("calculating the inverse & caching the result...")
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
