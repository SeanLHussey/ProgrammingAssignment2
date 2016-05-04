## Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the 
##inverse of a matrix rather than compute it repeatedly 
##Here are two functions that 1)creates a special object 
##that stores a matrix and 2) caches its inverse.


##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  MatInv <- NULL
  set <- function(y) {
    x <<- y
    MatInv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) MatInv <<- inverse
  getInverse <- function() MatInv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated and the 
## matrix has not changed, then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return inverse of matrix 'x'
  MatInv <- x$getInverse()
  if (!is.null(MatInv)) {
    message("Inverse already calculated, getting cached value")
    return(MatInv)
  }
  mat <- x$get()
  MatInv <- solve(mat, ...)
  x$setInverse(MatInv)
  MatInv
}
