##Input first the'x' as a matrix
##Then set "sv" as a null
##And change every reference "inverse" to "solvevalue"
##There are two functions makeCacheMatrix,makeCacheMatrix
##The function makeCacheMatrix contains set, get, setsolvevalue, getsolvevalue
makeCacheMatrix <- function(x = matrix()) {
  sv <- NULL
  set <- function(y){
    x <<- y
    ##initializing solved value as NULL
    sv <<- NULL
  }
  ##this function will get the matrix 'x' 
  get <- function() {x}
  setsolvevalue <- function(solvevalue) {sv <<- solvevalue}
  getsolvevalue <- function() {sv}
  list(set = set, get = get, setsolvevalue = setsolvevalue, getsolvevalue = getsolvevalue)
}

## Do the same procedure changing every "inverse" to "solvevalue"
##this function will solve the cache data
cacheSolveValue <- function(x, ...) {
  sv <- x$getsolvevalue()
  ##this function will determine whether the inverse is null
  if(!is.null(sv)) {
    message("getting cached data")
    ##this will return the solved value
    return(sv)
  }
  mat <- x$get()
  ##this is used to solve the solved value
  sv <- solve(mat, ...)
  x$setsolvevalue(sv)
  ##this will return the matrix of the solved value of the 'x'
  sv
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
