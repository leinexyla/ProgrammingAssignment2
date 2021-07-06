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
  ##this will return the matrix that is the inverse of the 'x'
  inv
}
