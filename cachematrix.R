makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y                   # Lexical scoping. So x will be accessible beyond this "set" function
    m <<- NULL                # Lexical scoping. So x will be accessible beyond this "set" function
  }
  get <- function() {         # It will contain the given matrix
    x
  }
  
  setSolve <- function(Solve) {
    m <<- Solve               # Lexical scoping. So x will be accessible beyond this "setSolve" function
  }                           # Repository for the inverse matrix
  getSolve <- function() {
    m                         # Repository for the inverse matrix
  }
  list(set = set, get = get,  # Return list objects
       setSolve = setSolve,
       getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
  m <- x$getSolve()           # Getting the inverse matrix from cache
  
  if(is.null(m)) {           # It will be a NA if there is no inverse matrix in the cache 
    data <- x$get()             # Getting the input matrix
    m <- solve(data, ...)       # solve a base function to inverse the input matrix
    x$setSolve(m)               # put the inverse matrix in the cache
  }
  else {
    message("getting cached data")   # If already inverse was stored in the cache.
  }
  return(m)                           # Return inverse matrix
}

# vec<- makeCacheMatrix()
# samplevector<- matrix(rnorm(3*3),3,3)
# vec$set(samplevector)
# vec$get()
# cacheSolve(vec)
# cacheSolve(vec)
