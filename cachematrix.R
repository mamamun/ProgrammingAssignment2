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
  }
  getSolve <- function() {
    m
  }
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

vec<- makeCacheMatrix()
samplevector<- matrix(rnorm(5*5),5,5)
vec$set(samplevector)
vec$get()
cacheSolve(vec)
cacheSolve(vec)
