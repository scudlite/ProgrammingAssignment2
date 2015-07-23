#creates a special "matrix", which is really a list containing a function 
#to set the matrix, get the matrix, set the inverse, get the inverse
makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) s <<- solve
     getInverse <- function() s
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}


## Return a matrix that is the inverse of 'x' where 'x' is the original argument for makeCacheMatrix
cacheSolve <- function(x, ...) {
        
     m <- x$getInverse()
#if the matrix inverse has been previously calculated, return the cached value
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
