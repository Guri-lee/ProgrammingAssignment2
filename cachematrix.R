###Programming assignment 2

#This  functions cache the inverse of a matrix:
#If it has been previously calculated (and the matrix has not changed), you can retrieve the inverse from the cache.
#If it has not been calculated, then the inverse is computed and cached.


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   #Sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x    #Gets the value of the matrix
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#The next function calculates the inverse of the matrix created before

cachesolve <- function(x, ...) {  #If the inverse has been already calculated, returns the value
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()                 #If it has not been calculated, calculate and sets the value in the cache
  m <- solve(data, ...)
  x$setsolve(m)
  m
}