
## This function checks to make sure that the matrix provided was of the right kind
## that can be inverted, before storing in the Cache

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <<- NULL
##
## Here the function ensures that all the elements of the matrix are numeric
## If they are not them it generates a warning message informint the error
##
  if (!is.numeric(x)){
    warning("Input Matrix is not numeric, so the matrix is not cached")
    return (NULL)
  }
##
## Here the function ensures that the matrix is a Square matrix
## If they are not them it generates a warning message informint the error
##
  else if (nrow(x) != ncol(x)){
    warning("Input Matrix is not a square matrix, so it is not being cached")
    return (NULL)
  }
##
## Here the function ensures that all the elements of the matrix are not only numeric
## but the values are properly assigned with valid numeric values
## If they are not them it generates a warning message informint the error
##
else if (sum(is.na(x)) > 0 || sum(is.nan(m)) > 0) {
    warning("Some Input Matrix element values equal to NA or NaN, so it is not being cached")
    return (NULL)
  } 
  
##
## Once it is established that matrix is a valid matrix it stores it in the cache
##

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
##
##  A get function retrieves the stored matrix
##
get <- function() x
##
## This function pushes the computed inverse matrix into the cache
##
  setInverse <- function(inverse) inverse <<- inverse
##
## This function pretrieves computed inverse matrix saved on the cache
##
getInverse <- function() inverse

##
## Here teh function returns the environment in which the cached R objects are stored
##

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}

##
## This function either computes the inverse matrix when invoked the first time
## On subsequent invocation it retrieves the stored inverse value and returns
##

cacheSolve <- function(x, ...) {
##
## Here the inverse value is retrieved and checked to see if the it has proper value
## if it is not seeded already it computes the inverse and stores it
##
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("Rerieved cached Inverse")
    return(inverse)
  }
##
## Here the functio, as there is not pre computed inverse value, retrieves the original matrix
##
  data <- x$get()
##
## Here the function first computes the determinant of the matrix to make sure that 
## it isn't zero.  If so then it issues a warning.
##
  if (det(data) == 0) {
    warning("The determinant of the Matrix is zero, so an inverse is not being computed")
    return(NULL)
  }
##
## Next it computes the inverse function and stores it in the cache and returns it
##

  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}