## makeCacheMatrix function creates a special "matrix" object(which is actually a list)
## It contains four basic functions which are-
## getMatrix : returns the matrix
## setMatrix : sets the matrix
## getMatInv : returns the Inverse of the matrix
## setMatInv : sets the Inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invertedMatrix <- NULL
  getMatrix <- function() x
  setMatrix <- function(y) x <- y
  getMatInv <- function() {
    invertedMatrix
  }
  setMatInv <- function(InvMat) {
    invertedMatrix <<- InvMat
  }
  list(getMatrix=getMatrix,setMatrix=setMatrix,getMatInv=getMatInv
       ,setMatInv=setMatInv)
}


## cacheSolve function takes a special "Matrix"(which is actually a list) as an argument
## At first, It checks if the inverse exists in the cache or not
## If it exists, then it fetches the inverse from the cache
## If not, then it calculates the inverse of the matrix & stores it in the cache


cacheSolve <- function(x, ...) {  
 
  if(!is.null(x$getMatInv())) {
    print("Fetching the inverse from the cache...")
    return(x$getMatInv())
  }
  else{
    print("Calculating the inverse & caching the result...")
  }
    invertedMat <- solve(x$getMatrix())
    x$setMatInv(invertedMat)
    invertedMat
}
