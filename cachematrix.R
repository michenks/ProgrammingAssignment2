## matrix inversion is usually a costly computation 
## there may be some benefits to caching the inverse of a matrix rater than compute it repeatedly
## below are functions used to create a special object that stores a matrix and cache its inverse.

## this function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function computes the inverse of the special matrix created by maleCacheMatrix above.
## if the inverse has already been calculated (and the matrix has not changed),
## then it should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse
   if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
        
   }
   mat <- x$get()
   inv <- solve(mat, ...)
   x$setInverse(inv)
   inv
}
