##Input 'x' as a matrix
##There are two functions makeCacheMatrix,CacheMatrix
##The function makeCacheMatrix contains set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    ##initializing inverse as NULL
    inv <<- NULL
  }
  ##this function will get the matrix 'x' 
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##this function will solve the cache data
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ##this function will determine whether the inverse is null
  if(!is.null(inv)) {
    message("getting cached data")
    ##this will return the inverse value
    return(inv)
  }
  mat <- x$get()
  ##this is used to solve the inverse value
  inv <- solve(mat, ...)
  x$setinverse(inv)
  ##this will return the matrix of the inverse of the 'x'
  inv
}

## ---------------To Check the Program------------------------
##m <- matrix(rnorm(16),4,4)
##m1 <- makeCacheMatrix(m)
##cacheSolve(m1)

##            [,1]        [,2]       [,3]        [,4]
##[1,] -0.39392557 -0.15720316  1.0575576 -1.35866156
##[2,]  0.09471497 -0.01106366 -0.8365244 -0.00102469
##[3,]  0.45029486 -0.51613005 -1.2293369  2.00323308
##[4,]  0.38921950 -0.89141814 -2.7510053  4.17160934
