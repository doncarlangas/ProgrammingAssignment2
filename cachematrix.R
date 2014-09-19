## These functions allow us to compute the inverse of a matrix once and retrieve it from the cache 
## later without computing it again


makeCacheMatrix <- function(x = matrix()) {

    # Set initial value for the inverse matrix
    i <- NULL
    
    # set the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set the inverse matrix
    setinv <- function(inv) i <<- inv
    
    # get the inverse matrix from cache
    getinv <- function() i
    
    # output of the function
    list ( set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
    # return the inverse matrix from the cache
    i <- x$getinv()
    
    #If the inverse matrix has already been computed, it returns the cached data
    if(!is.null(i)) {
         message("getting cached data")
         return(i)
    }
    
    #calculate inverse matrix
    data <- x$get()
    i <- solve(data, ...)
    
    #set computed inverse matrix
    x$setinv(i)
    
    #return inverse matrix
    i
}
