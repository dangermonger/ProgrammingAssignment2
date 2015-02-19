## makeCacheMatrix comprises a list of functions that can cache or
## retrieve an original or inverted matrix. cacheSolve checks to  
## see if the current matrix has already been inverted and if so,  
## the inverse is retrieved from the cache rather than being 
## calculated again. Otherwise, the matrix is inverted.

## makeCacheMatrix creates a list of functions that can cache and 
## retrieve a matrix or an inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL ## inverseMatrix is initially set to zero (default value)
  set <- function(y) { ## the given matrix is cached
    x <<- y
    inverseMatrix <<- NULL
  } 
  get <- function() x ## retrieves cached matrix
  
  setinverse <- function(inverse) inverseMatrix <<- inverse ## caches the inverted matrix
  getinverse <- function() inverseMatrix ##retrieves inverted matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##binds list elements to functions defined in makeCacheMatrix
}

## calling functions from makeCacheMatrix, cacheSolve checks 
## if the given matrix has already been inverted, if not it inverts it.

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getinverse() ## assign inverseMatrix to cached inverted matrix
  if(!is.null(inverseMatrix)) { ## if inverseMatrix is not at its default value, -
    return(inverseMatrix)       ## return cached inverseMatrix.
  }
  
  originalmatrix <- x$get() ##assign originalmatrix to the original matrix
  inverseMatrix <- solve(originalmatrix, ...) ##assigns invertMatrix to inversion of matrix
  inverseMatrix ##returns inverted matrix
}
