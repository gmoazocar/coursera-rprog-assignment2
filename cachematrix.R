# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a 
# matrix rather than compute it repeatedly

# the following two functios are used to cache the inverse of a matrix

# makeCacheMatrix will have the following steps:

# 1. set the value of the matrix

# 2. get the value of the matrix

# 3. set the value of inverse of the matrix

# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  cacheSolve: This function computes the 
#   inverse of the special "matrix" returned
#   by makeCacheMatrix above. If the inverse
#   has already been calculated 
#  (and the matrix has not changed),
#  then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

test1 <- matrix(c(1,2,3,4),2,2)
test2 <- makeCacheMatrix(test1)
cacheSolve(test2)
