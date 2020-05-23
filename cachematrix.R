## Create tool to cache of solution of a matrix
## Usage: cacheSolve(makeCacheMatrix(m1))
## Where m1 is source matrix

## makeCacheMatrix() stores matrix in it's environment and provides ways to cache solution of that matrix

makeCacheMatrix <- function(x=matrix()){
  ##x is initialized in function call, initialize m (future cache)
  m <- NULL
  ##set: initialize y and reassign vallue of x in parent environment; clear cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get: return x
  get <- function() x
  ##setsolve: initialize solution of the matrix to keep it cached in m
  setsolve <- function(solve) m <<- solve
  ##getsolve: return cached m
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Return a matrix that is the inverse of 'x'. Uses cache of makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Initiate m to be checked from cache
  m <- x$getsolve()
  if(!is.null(m)){
    message("Solved matrix found in cache")
    return(m)
  }
  ## If cache is empty, solve matrix and store it in cache
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
  m
}
