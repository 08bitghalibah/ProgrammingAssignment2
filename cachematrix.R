## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Stores the inverse of the matrix 
#returns the matrix inverse if it exists and it's of same matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <- y
    invmatrix <<- NULL
  }
  get <- function() x
# sets the inverse of the matrix in the cache
  setinverse <- function(inmatrix) invmatrix <<- inmatrix
  getinverse <- function() invmatrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}


## Write a short comment describing this function
# compute the inverse of the matrix if it's does not exists in cache or it's different from the previous
# uses the solve() function to calculate inverse
# Assume that matrix is always invertible
cacheSolve <- function(x, ...) {
# Get a matrix that is the inverse of 'x'
  invmatrix <- x$getinverse()
#checks that inverse is already cached or not 
  if(!is.null(invmatrix)) {
    message("getting cached inverse")
    return(invmatrix)
  }
# compute inverse if not cached
  data<-x$get()
#takes inverse of the matrix
  invmatrix<-solve(data)
  x$setinverse(invmatrix)
  invmatrix
}
# test code for testing the result
x <- makeCacheMatrix(matrix(1:4,2))
cacheSolve(x)
cacheSolve(x)
