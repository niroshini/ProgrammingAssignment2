## author: niroshinie fernando

##  Example Usage: 
## A <- matrix(c(1,4,3,4,5,6,7,8,90),nrow=3, ncol = 3, byrow = TRUE)
## C<- makeCacheMatrix(A)
## cacheSolve(C)

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inMat <- NULL
  set <- function(y) {
    x <<- y
    inMat <<- NULL
  }
  get <- function() x
  setInMat <- function(pInmat) inMat <<- pInmat
  getInMat <- function() inMat
  list(set = set, get = get,
       setInMat = setInMat,
       getInMat = getInMat)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inMat <- x$getInMat()
  if(!is.null(inMat)) {
    message("getting cached data")
    return(inMat)
  }
  data <- x$get()
  inMat <- solve(data, ...)
  x$setInMat(inMat)
  inMat
}
