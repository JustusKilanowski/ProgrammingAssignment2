## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##inverse as NULL
##Function to get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { ##receive the cache data
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) ##returns inverse value
  }
  data <- x$get()
  inv <- solve(data, ...) ##calculates inverse value
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
