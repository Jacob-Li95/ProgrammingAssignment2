## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matix"object that can cache it's inverse.


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <-function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
    }


## Write a short comment describing this function
## This functioncomputes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse have already been calculated and the matrix haven't changed, then the cachesolve retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

