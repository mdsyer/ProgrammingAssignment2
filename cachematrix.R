## The CacheMatrix "object" caches the result of a matrix
## inverse (a computationally intense operation) and the 
## cacheSolve function demonstrates how the CacheMatrix
## "object" may be used in a computation.

## Makes a CacheMatrix "object" with a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # makeCacheMatrix fields
  inverse <- NULL
  
  # check whether x is a matrix
  if(!is.matrix(x)) {
    stop("x is not a matrix")
  }
  
  # set the makeCacheMatrix matrix
  set_matrix <- function(y) {
    x <<- y    
    inverse <<- NULL
  }
  
  # set the makeCacheMatrix matrix inverse
  set_inverse <- function(x_inverse) {
      inverse <<- x_inverse
  }

  # get the makeCacheMatrix matrix
  get_matrix <- function() {
    return(x)
  }
  
  # get the makeCacheMatrix matrix inverse
  get_inverse <- function() {
    return(inverse)
  }
  
  # collect the makeCacheMatrix functions
  list(get_matrix  = get_matrix,
       get_inverse = get_inverse,
       set_matrix  = set_matrix,
       set_inverse = set_inverse)
}


## The cacheSolve function demonstrates how the CacheMatrix
## "object" may be used in a computation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # check whether the inverse has already been cached
    inverse <- x$get_inverse()
    if(is.null(inverse)) {
        # calculate, set and return the inverse if
        # the inverse is not cached
        x$set_inverse(solve(x$get_matrix()))
        return(x$get_inverse())
    }
    # return the cached inverse
    return(inverse)
}
