## The goal of these two functions is to cache the inverse of a matrix. Cacheing a matrix is used
## to reduce the processing load on a computer by cacheing (saving) the matrix results, avoiding the
## computational time of performing the same step (in our case, inverting a matrix) multiple times.
## Note: as per the assignment instructions, this file assumes that any matrix supplied is invertible.

## This first function takes a matrix argument, inverts it, and caches the original and inverted values.

makeCacheMatrix <- function(x = matrix()) {
  inver = NULL
  createm <- function(y){
    x <<- y
    inver <<- NULL
  }
  retriever <- function() x
  setinverse <- function(inverse) inver <<- inverse
  getinverse <- function() inver
  list(createm=createm, retriever=retriever, setinverse=setinverse, getinverse=getinverse)
  
  
}
## This function first checks to see if the cached matrix has already been inverted. If it has, it returns inver.
## If it has not, it retrieves the matrix and inverts it.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)) {
    message("getting cached data.")
    return(inver)
  }
  inver <- solve(x$retriever())
  x$setinver(inver)
  inver
}