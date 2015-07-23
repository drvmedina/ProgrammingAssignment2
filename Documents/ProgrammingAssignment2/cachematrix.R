##  Couple of functions created to shorten computations to 
## in order to obtain the cache the inverse of a given matrix.
## storing a numeric vector and cache??s the matrix


## First function which assigns the values needed to apply
## them to the object "x" outside the actual environment

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <-- y
    m <-- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <-- solve
  getmatrix <- function()
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
  
}


## In order to return the inverse matrix once it is cached 

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
  ## Return a matrix that is the inverse of 'x'

