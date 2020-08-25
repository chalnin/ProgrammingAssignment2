## Coursera R Programming Week 3 Assignment 
## Pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix) {   ## defines the makeCacheMatrix function with a default mode of matrix 
  i <- NULL                               ## i holds the value of the inverse of the matrix with an initial value of NULL
  set <- function (y){                    ## define the set function with a new value
    x <<- y                               ## matrix value in the parent environment
    i <<- NULL                            ## reset i to NULL if there's a new matrix
  }
  get <- function() x                     ## returns the value of the matrix argument
  setinv <- function(inv) i <<- inv       ## assigns value of i in the parent environment
  getinv <- function() i                  ## gets the value of i
  list (set = set, get = get,             ## needed for the function with $ operator
        setinv =setinv,
        getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {         ## returns matrix that is inverse of x
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinv(i)
  i
}

