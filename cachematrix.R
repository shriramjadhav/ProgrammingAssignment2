## This R file provides two functions
##  "makeCacheMatrix" : to cache the inverse of matrix
##  "cacheSolve" : to lookup in cache, if not found calculate inverse of matrix

## Following function creates special matrix object and provides following functionality
##  1. get matrix
##  2. set matrix & internally unset the inverse of matrix when provided matrix does not match with old one
##  3. setInverse of matrix
##  4. getInverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseM <- NULL
  set <- function (y) {
    # Check if the matrix has not changed
    #Ref : https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
    if (!(is.matrix(x) &&
          is.matrix(y) && dim(x) == dim(y) && all(x == y))) {
      inverseM <- NULL
    }
    x <<- y
  }
  
  get <- function () {
    x
  }
  
  setInverse <- function (inverse) {
    inverseM <<- inverse
  }
  
  getInverse <- function () {
    inverseM
  }
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Following function checks whether inverse exits in cache otherwise ...
##  calculates the inverse of the matrix provided by function "makeCacheMatrix"
## Returns the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverse()
  if (!is.null(inverseM)) {
    message("getting cached data")
    return(inverseM)
  }
  data <- x$get()
  inverseM <- solve(data, ...)
  x$setInverse(inverseM)
  inverseM
}
