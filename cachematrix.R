##makeCacheMatrix creates a special "matrix" object that can cache its inverse
##the input matrix is a square invertible matrix
##
makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                                    ##sets the inverse to NULL
  set <- function(y) {    
                                                 ##using '<<-' to assign a value to an object in an 
                                                 ##environment different from the current environment
  x <<- y                                        ##defines a function to set matrix, x, to a new matrix, y
  inv <<- NULL                                   ##resets the inverse to NULL
  }
get <- function() x                              ##returns the matrix x
setinver <- function(inverse)  inv <<- inverse   ## sets the inverse, inv, to inverse
getinver <- function() inv                       ## returns the inverse, inv
list(set = set, get = get, setinver = setinver, 
     getinver = getinver)                        ## this list is used as the input to cacheSolve
}

##cacheSolve computes the inverse of the “matrix” returned by makeCacheMatrix(). 
##If the inverse has already been calculated and the matrix has not changed,
##it retrieves the inverse from the cache directly.
##
cacheSolve <- function(x,...) {                  ## Here, x is the output of the makeCacheMatrix()
        inv <- x$getinver()       
        if(!is.null(inv)) {
                message("getting cached data") 
                return(inv)                      ##inverse of the original matrix input to makeCacheMatrix()
        }
        data <- x$get()                          ##If the inverse has not already been calculated and is not 
        inv <- solve(data, ...)                  ##found in the cache, the input matrix is used and the
                                                 ##inverse is calculated 
        x$setinver(inv)                          ##sets the value of the inverse in the cache
        return(inv)
}
