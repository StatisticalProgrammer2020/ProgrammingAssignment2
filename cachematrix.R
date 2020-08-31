## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The following function creates a special "matrix" object which is really a list containing a function to
# set the values of the matrix (set)
# get the values of the matrix (get)
# set the values of the inversed matrix (set_inverse)
# get the values of the inversed matrix (get_inverse)

makeCacheMatrix <- function(x = matrix()) {
  inversed_matrix <- NULL
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }
  get <- function() x
  set_inverse <- function(solve) inversed_matrix <<- solve
  get_inverse <- function() inversed_matrix
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  inversed_matrix <- x$get_inverse()
  if(!is.null(inversed_matrix)) {
    message("getting cached data")
    return(inversed_matrix)
  }
  data <- x$get()
  inversed_matrix <- solve(data, ...)
  x$set_inverse(inversed_matrix)
  inversed_matrix
}
