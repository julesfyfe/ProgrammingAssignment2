## This pair of functions can be used to cache a matrix and
## its inverse

## The first function parses in a matrix to create a list comprising
## functions to  set and retrieve the cached matrix (from the global env),
## and set and retrieve the cached inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
      ## Set the internal inv variable to NULL
      inv <- NULL
      ## function to cache an alternative matrix
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      ## Function to return the input matrix
      get <- function() {
            x
      }
      ## Function to superassign the inverse matrix variable to parent or global
      ## environment
      setinv <- function(inverse) {
            inv <<- inverse
      }
      ## Function to return the cached inverse matrix
      getinv <- function() {
            inv
      }
      ## return a list of the functions
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)        
}

## This second function accesses the object created by makeCacheMatrix
## by fetching and returning the inverse, or if the inverse is NULL,
## calculate the inverse, cache it and return it

cachesolve <- function(x, ...) {
      ## retrieve the inverse matrix from the object x
      inv <- x$getinv()
      ## Check the matrix returned is not NULL, return it
      if(!is.null(inv)) {
            message("getting cached matrix")
            return(inv)
      }
      ## if inv is NULL, calculate the inverse matrix
      data <- x$get()
      inv <- solve(data)
      ## Assign inv to cache
      x$setinv(inv)
      ## Return calculated inverse
      inv
}

## Testing out the code

hilbert <- function(n) { i <- 1:n
                         1 / outer(i - 1, i, "+")
}
h8 <- hilbert(8)
test1 <- makeCacheMatrix(h8)
cachesolve(test1)

set.seed(77)
m2 <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
test2 <- makeCacheMatrix(m2)
cachesolve(test2)

m3 <- matrix(sample.int(1000000,size=250000,replace=TRUE), nrow=500)
test3 <- makeCacheMatrix(m3)
cachesolve(test3)
test3$set(m3)
system.time(cachesolve(test3))
system.time(cachesolve(test3))
