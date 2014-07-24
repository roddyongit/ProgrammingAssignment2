## This R file contains two functions:
##
## 1. makeCacheMatrix
##          a) Recieves a matrix as input paramter         
##          b) Declares a variable 'm' to store the results of an inversed 
##             matrix
##          c) Declares 4 method to:
##                  i. Get the new non-cached matrix: "get"
##                 ii. Get the already cached matrix: "getsolve"
##                iii. Set the initial value of the matrix to invert: "set"
##                 iv. Calculate the inverse of the matrix if not 
##                     already cached: "setsolve"
## 2. cacheSolve
##        This function takes a matrix 'x' as input parameter
##        checks whether a similar matrix is already stored in the global env.
##        If a matrix = x is found in the global env, then the already cached
##        inverse of the matrix is returned (see statamente return(m) with 
##        no need to recalculate it with the statement m <- solve(data, ...)
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## This function receive a matrix as input parameter and
## declares the get and set method to retrieve or calculate the
## inverse of the matrix passed in the argument
## ---------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve) 
        
}

## -------------------------------------------------------------------
## This function returns cached matrix inverse using computed matrix
## in the makeCacheMatrix function
## -------------------------------------------------------------------
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()   
        if(!is.null(m)) {   
                message("getting cached data")
                return(m)      ## Returns the matrix previously cached.
        }
        ## -------------------------------------------------------------------                       
        data <- x$get()        
        m <- solve(data, ...)  ## Calculates the inverse of the matrix 
                               ## and stores the results in m
        x$setsolve(m)          
        m                      
}
