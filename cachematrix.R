## These functions calculate the Inverse of a Matrix and cache
## the result.  If a new matrix is loaded, the cache is cleared
## and the inverse of the new matrix is calculated

## This function creates a list made up of four functions
#Set assigns a matrix to this function
#Get retrieves the matrix currently stored
#Set Inverse is used to pass an inverse matrix to this list.
#It is used by cacheSolve
#Get Inverse retrieves the inverse currently stored in cache

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL 
  set <- function(y) { 
    x <<- y 
    I <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(Inverse) I <<- Inverse 
  getInverse <- function() I 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse) 
}


## This function calculates the inverse of the matrix stored
#in the list created by makeCacheMatrix.  If the inverse has already
#been calculated it pulls the cached version from that same list.
#If not it calculates the inverse and stores it in that list for future use.


cacheSolve <- function(x, ...) {
  I <- x$getInverse() 
  if(!is.null(I)) { 
    message("getting cached data") 
    return(I) 
  } 
  data <- x$get() 
  I <- solve(data, ...) 
  x$setInverse(I) 
  I 
}
