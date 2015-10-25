## The functions makeCacheMatrix and cacheSolve calculate the inverse for any
## matrix for which the calculation is possible. The inverse is stored in cache
## and returned when required, without the solve() function being run every time
## the cachSolve function is called with a matrix.

## This function contains four member functions that set the original value of the
## inverse to NULL and stores the calculated value when the solve() function is
## called in the cacheSolve function. It also depicts lexical scoping where the
## values assigned to variables within the function are also applicable outside the
## function environment.
  
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solution) inv <<- solution
  getinv  <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function takes in the makeCacheMatrix function as a parameter and checks
## whether the inverse is already stored in cache. It returns the stored value if 
## present and the calculated value if not present.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

test <- matrix(1:4,2,2)
a <- makeCacheMatrix()
a$set(test)
cacheSolve(a)

