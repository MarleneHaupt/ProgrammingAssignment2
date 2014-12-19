# Example Assignment 2

# Caching the Mean of a Vector

# makeVector is a list containing a function
# stores 'x' and 'm'

makeVector <- function(x = numeric()) {             
# input x will be a vector
  
      m <- NULL
# m will be our 'mean' and it's reset to NULL every time makeVector is called      
      
      set <- function(y) {
# takes an input vector

      x <<- y
# saves the input vector

      m <<- NULL
# reset the mean to NULL
}
      
      get <- function() {x}
# this function returns the value of the original vector
      
      setmean <- function(mean) {m <<- mean}
# called by cachemean() during the first cachemean() access and it will store the value using superassignment

      getmean <- function() {m}
# this will return the cached value to cachemean() on subsequent accesses

      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)  
# accessed each time makeVector() is called, so each time we make a new object
# it is a list of the internal functions (methods) so a calling function knows how to access those methods

}

cachemean <- function(x, ...) {
# the input x is an object created by makeVector

      m <- x$getmean()
# access the object 'x' and get the value of the mean

      if(!is.null(m)) {
# if mean was already cached (is not NULL)
        
              message("getting cached data")
# send this message to the console

              return(m)
# and return the mean... return ends the function cachemean
      }

      data <- x$get()
# we reach this code only if x$getmean() returned NULL
      
      m <- mean(data, ...)
# if m is NULL we have to calculate the mean

      x$setmean(m)
# store the calculated mean value in x

      m
# return teh mean to code that called this function
}


# Examples
bigVec <- makeVector(1:1000)
biggerVec <- makeVector(1:100000)

cachemean(bigVec)
cachemean(bigVec)
cachemean(biggerVec)
cachemean(biggerVec)

bigVec <- makeVector(1:2)

cachemean(bigVec)
cachemean(bigVec)

bigVec$get()
bigVec$getmean()
biggerVec$get()
biggerVec$getmean()

bV <- makeVector(1:10)
cachemean(bV)
cachemean(bV)
bV$get()

bV$set(50:60)
cachemean(bV)
cachemean(bV)

bV$get()

# The operator <<- ar normally only used in functions
# cause a search to made through parent environments for an existing definition 
# of the variable being assigned. If such a variable is found then its value is 
# redefined, otherwise assignment takes place in the global environment
# their semantics differ from that in the S language, but are useful in 
# conjunction with the scoping rules of R