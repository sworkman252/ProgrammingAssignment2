##makeCacheMatrix and cacheSolve work together to; first, see if the inverse 
##of a given matrix has already been cached, if so return the cached value
##if the inverse of the given matrix has not already been calculated and
##cached to do so and cache the value for future calls of the functions

##First, makeCacheMatrix creates a list of a functions to be called later,
##as well as creates the objects x(our matrix) and inv(the inverse of x)
##inv is first set to be NULL

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

##cacheSolve first checks to see if the inverse of the given matrix has been
##stored in the cache using the get and getinverse functions from
##makeCacheMatrix.  if the value of inv has already been cached it is printed,
##along with the message "getting cached data".  If the value has not already
##been cached it is calculated and cached using the solve and setinverse functions

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}