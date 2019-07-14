## Functions to create and handle a special "matrix" and its inverse.
## 

## Creates a special "matrix", which is a list containing a function to get and set the matrix
##  and get and set the invers of that matrix

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


## The following function calculates and return the inverse of the special "matrix" created with the above function
## assumes there is an invers to the matrix x
## once called caches the inverse for further use

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
