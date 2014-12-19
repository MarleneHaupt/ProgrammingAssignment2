## Assignment 2

## Caching the Inverse of a Matrix

## makeCacheMatrix is a matrix containing a function
## stores 'x' and 'm'

makeCacheMatrix <- function(x = matrix()) {    
# input x will be a matrix

                    m <- NULL
                    # m will be our 'inverse' and it's reset to NULL every time makeCacheMatrix is called      
  
                    set <- function(y) {
                    # takes an input matrix
    
                            x <<- y
                            # saves the input matrix
    
                            m <<- NULL
                            # reset the inverse to NULL
                    }
  
  get <- function() {x}
  # this function returns the value of the original matrix
  
  setsolve <- function(solve) {m <<- solve}
  # called by cacheSolve() during the first cacheSolve() access and it will store the value using superassignment
  
  getsolve <- function() {m}
  # this will return the cached value to cacheSolve() on subsequent accesses
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
  # accessed each time makeCacheMatrix() is called, so each time we make a new object
  # it is a list of the internal functions (methods) so a calling function knows how to access those methods
  
}


cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
   
# the input x is an object created by makeCacheMatrix
  
  m <- x$getsolve()
  # access the object 'x' and get the inverve
  
  if(!is.null(m)) {
    # if inverse was already cached (is not NULL)
    
    message("getting cached data")
    # send this message to the console
    
    return(m)
    # and return the inverse... return ends the function cacheSolve
  }
  
  data <- x$get()
  # we reach this code only if x$getsolve() returned NULL
  
  m <- solve(data, ...)
  # if m is NULL we have to calculate the inverse
  
  x$setsolve(m)
  # store the calculated inverse
  
  m
  # return the inverse to code that called this function
}



################################################################
# Example 1
set.seed(77)
m <- matrix(sample.int(100, size = 9, replace = TRUE), nrow = 3)
m

inverse <- makeCacheMatrix(m)
cacheSolve(inverse)

# Example 2
set.seed(77)
m <- matrix(sample.int(100, size = 16, replace = TRUE), nrow = 4)
m

inverse <- makeCacheMatrix(m)
cacheSolve(inverse)

# Example 3
set.seed(77)
m <- matrix(sample.int(100, size = 10000, replace = TRUE), nrow = 100)
m

inverse <- makeCacheMatrix(m)
system.time(cacheSolve(inverse))
system.time(cacheSolve(inverse))

# Example 4
set.seed(77)
m <- matrix(sample.int(100, size = 9, replace = TRUE), nrow = 3)
m

inverse <- makeCacheMatrix(m)
cacheSolve(inverse)

inverse$set(m)
cacheSolve(inverse)

#Example 5
marlene <- matrix(c(5, 5, 6, 10), 2, 2)
marlene

inversemarlene <- makeCacheMatrix(marlene)
cacheSolve(inversemarlene)