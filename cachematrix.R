## This project calculates the inverse of a square matrix. Each calculation is cached.
## If the inverse has already ben calculated, the cached result is returned instead of re-calculating

## makeCacheMatrix creates a table of functions:
##    "set" collects the matrix you are inverting for the function and 
##        checks to make sure the input is  a square matrix
##    "get" reads the matrix out
##    "setSolve" stores solution
##    "getSolve" reads the solutions
## To use it, Call the makeCacheMatrix() function and assign it's
## return value (a list of four functions) to a variable, such as 'v'
## by using v <- makeCacheMatrix()
##  v is now a list of the four functions

makeCacheMatrix <- function(x = matrix()) {   ## Create the table of functions
  m <- NULL
  set <- function(y) {                        ## Create set function
      x <<- y
      if(is.matrix(x)==FALSE){stop("Not a Matrix!")}      ## Tests to make sure input is a Matrix
      if(ncol(x)!=nrow(x)){stop("Not a SQUARE Matrix!")}  ## Tests to make sure input is a SQUARE Matrix
      m <<- NULL
    }
    get <- function() x                         ## Creates get function
    setSolve <- function(solve) m <<- solve     ## Creates setSolve function
    getSolve <- function() m                    ## Creates getSolve function
  ## Create the list:
    list(set = set, get = get,
        setSolve = setSolve,
        getSolve = getSolve)

}


## cacheSolve checks to see if the inverse has already been calculated
## if it has, it returns the cached value
## if it has not, it calls the "solve()" function in R to invert the matrix, and returns that matrix, caching it

cacheSolve <- function(x, ...) {
                                                ## Return a matrix that is the inverse of 'x'
   m <- x$getSolve()                            ## call getSolve and send to m
      if(!is.null(m)) {                         ## check to see if the cached answer exists
      message("getting cached data")            ## If so, alert the user that the value is cached
      return(m)                                 ## return the cached solution
    }
    data <- x$get()                             ## Assign the output of get to data
    m <- solve(data, ...)                       ## Call solve() to calculate the inverse
    x$setSolve(m)                               ## call setSolve to store the result
    m                                           ## print m

}
