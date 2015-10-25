# Put comments here that give an overall description of what your
# functions do
# These two functions will create, store and recall a matrix and the inverse from cache 
# Write a short comment describing this function

# makeCacheMatrix <- function(x = matrix()) {

# This function creates matrix of the type capable of running four functions
# set stores the matrix in cache and get recalls the matrix
# setInverse and getInverse do basically the same thing, but for the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m<<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) 
# cacheSolve takes a custom matrix type created by the makeCacheMatrix function
# and calculates the inverse matrix of it. It does so by first checking to see if 
# the calculation has been done.If the calculation has been done before, it recalls
# the data from the cache. If however, the calculation has not been done before, it calculates
# the inverse matrix and stores it in cache.
  
cacheSolve <- function(x, ...) 
# Return a matrix that is the inverse of 'x'
cacheSolve <-function(x,...)  {
      m <-x$getInverse()
      if(!is.null(m)){
        message("getting cache data")
        return(m)
      }
      data <- x$get()
      m <- solve(data,...)
      x$setInverse(m)
      
      
    }

