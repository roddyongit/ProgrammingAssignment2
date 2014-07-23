## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

tmakeCacheMatrix <- function(x = matrix()) {

            xo <<- x   ## Store original value to a global variable xo    
            ml <- solve(x)  ## invert the square matrix
            mg <<- ml  ## Store converted matrix to a global variable mg    
            
            ml    ## return result from function
                    
#         m <- NULL
#         set <- function(y) {
#                 x <<- y
#                 m <<- NULL
#         }
#         get <- function() x
#         setsolve <- function(solve) m <<- solve
#         getsolve <- function() m
#         list(set = set, get = get,
#              setsolve = setsolve,
#              getsolve = getsolve)

            
}


## Write a short comment describing this function

tcacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m  
        ## data  ## testing output of inversed matrix
        
}
