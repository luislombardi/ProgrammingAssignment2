## Assignment: Caching the Inverse of a Matrix
## For this assignment, we assume that the matrix supplied is always invertible.
## The assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix function
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

## This function creates a matrix object that can cache its inverse.
## Let X be a square n by n matrix and X is always invertible.
## Based in the explanation of "makeVector" given by Bill Hilton

makeCacheMatrix <- function(x = matrix()) {      # input x will be a matrix
  
  I <- NULL    #  I is the inverse matrix  and it is reset with any call to makeCacheMatrix 
  
  #   The next three functions are not run when makeMatrix is called.
  #   instead, they will be used by cacheSolve() to get values for x or for
  #   I (inverse) and for setting the inverse.  These are usually called object 'methods'
  
  set <- function(y) {               # takes an input matrix
    x <<- y                          # saves the input matrix   
    m <<- NULL                       # resets the matrix to NULL, basically what happens when a new object is generat  
  }
  
  get <- function() { x }            # this function returns the value of the original matrix
  
  setInverse <- function(inverse)    # this is called by cacheSolve() during the first cacheSolve()
             { I <<- inverse }       #  access and it will store the value using "superassignment"
  
  getInverse <- function() { I }     # this will return the cached value to cacheSolve() on
                                     #  subsequent accesses
  
  list(get = get,                    #  This list is returned with the newly created object.       
       setInverse = setInverse,      #   It lists all the functions ("methods") that are part of
       getInverse = getInverse)      #   the object.  If a function is not on the list then it cannot
                                     #   be accessed externally.
}
  

## cacheSolve function
## Return a matrix that is the inverse of 'x'
## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.

cacheSolve <- function(x, ...) {
  im <- x$getInverse()                # access to the makeCacheMatrix function
  if(!is.null(im)) {                  # if inverse matrix was already cached (not NULL) ...   
    message("... cached matrix.")     #  send this message to the console  
    return(im)                        #  and return the inverse matrix (Ends the function)
  }
  data <- x$get()                     # the inverse has not been computed then 
  im <- solve(data)                   # calculate the inverse matrix
  x$setinverse(im)                    # store the calculated inverse matrix
  im                                  # and return  the inverse matrix   
}

  
   
  ## A simple test:
  ## > B = matrix(c(1,2,3,4), nrow=2, ncol=2 ) 
  ## > M = makeCacheMatrix(B)
  ## > M$get()
  ##       [,1]  [,2]
  ## [1,]  1      3 
  ## [2,]  2      4  
  
  ## No cache in the first run
  ## > cacheSolve(B)
  ##       [,1]   [,2]
  ## [1,]  -2     1.5
  ## [2,]   1    -0.5   
  
  ## Retrieving from the cache in the second run
  ## > cacheSolve(B)
  ## getting cached data.
  ##        [,1]   [,2]
  ## [1,]   -2     1.5
  ## [2,]    1    -0.5  
  
  ## It's really the inverse?
  ## > M$get() %*% cacheSolve(M) 
  ##getting cached data.
  ##       [,1] [,2]
  ##[1,]    1    0
  ##[2,]    0    1
  ## It seems so...

  