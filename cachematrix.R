## There are two functions, which help to compute the inverse of a matrix and cache it. 

## This first function takes a numeric n x n matrix as its argument and creates a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
      # Creates a null object where the inverted matrix can later be accommodated (= "the empty matrix object that can cache its inverse")
      inv <- NULL
      
      # A function that lets the user change the input matrix and when this is done, the chached inverse of the previous matrix is set to NULL (i.e. the cache is emptied). Both are done outside the environment of this function (hence the <<-).
      set <- function(y) { 
            x <<- y
            inv <<- NULL
      }
      
      # A function that shows which matrix is currently used
      get <- function() x
      
      # Accommodates the inverted matrix in the empty object inv. This, too, is done outside the environment of this function (hence again the <<-).
      invert <- function(solve) inv <<- solve 
      
      # A function that shows whether there is a cached version of an inverted matrix
      getinverted <- function() inv 
      
      # Makes a list of all the functions created (which can be accessed with $-symbol)
      list(set=set, get=get, invert=invert, getinverted=getinverted) 
} 


## The other function checks, whether there is a cached inversion of a matrix. If yes, then the inverted matrix is returned. If not, a matrix will be inversed and the inverted matrix will be cached.

cacheSolve <- function(x) {
      # Checks for a cached inversion of a matrix
      inv <- x$getinverted()
      
      # If the inverse of a matrix has already been calculated, the cached inverted matrix is returned and nothing else follows.
      if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      
      # If the cache is empty, a matrix is fetched, ...
      data <- x$get()
      
      # ... inverted...
      inv <- solve(data)
      
      # ... and cached
      x$invert(inv)
}