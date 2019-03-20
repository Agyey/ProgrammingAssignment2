## makeCacheMatrix creates a special matrix vector with 4 
## utility functions, get, set, getinv, setinv

## cacheSolve checks if inverse of a matrix is cached
## if it is, then it returns the cached value
## else it calculates the inverse and stores it



## This function takes in a matrix as an argument
## and it returns a list of functions;
## set,get,setinv,getinv
## set takes the input argument and assigns it to x
## get returns the value stored in x
## setinv takes the input argument and assigns it to invm
## getinv returns the value store in invm

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invm <<- inv
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## This function takes in a list of functions
## returned by makeCacheMatrix
## it checks it the given matrix has its inverse
## cached, if it is then it returns the cached value
## else it calculated the inverse and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data, ...)
  x$setinv(invm)
  invm
}
