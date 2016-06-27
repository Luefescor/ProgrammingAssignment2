
##  The two functions are used to create a special object that stores a matrix 
## and caches its inverse.

## The first function, makeCacheMatrix(), creates a special "matrix", which is really a list 
## containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the matrix inverse
##4. get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
      I <- NULL
      set <- function(y) {
        x <<- y
        I <<- NULL
      }
      get <- function() x
      setInverse <- function(Inverse) I <<- Inverse
      getInverse <- function() I
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function, first checks to see if the matrix inverse has already been 
## calculated, so in afirmative case it gets the inverse from the cache and skips the 
## computation, and in negative case, it calculates the matrix invers and sets the
## inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached Matrix")
    return(I) 
  }
  Matrix <- x$get()
  I <- solve(Matrix, ...)
  x$setInverse(I)
  I  
}
# ProgrammingAssignment2
