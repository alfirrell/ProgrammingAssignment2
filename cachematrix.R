## Use makeCacheMatrix and cacheSolve to cache matrix inverses, so repeated
## calls for the same (slow-to-compute) inverse will run faster

## - makeCacheMatrix generates a list of cache-management functions,
## to get and set the matrix value and the value of its inverse
## - cacheSolve uses those functions, and if there is no cached inverse
## it does the solve(), caches the result for next time, and returns the value


## makeCacheMatrix:
## This function manage the cache - getters and setters for x (the base matrix)
## and inv (the inverse matrix)

makeCacheMatrix <- function(x = matrix()) {
    
    ## initialise inv to NULL
    inv <- NULL
    
    ## when a new base matrix x is supplied, save it to x 
    ## and clear out the inverse
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    get <- function() x
    
    ## get/set the inverse
    setSolve <- function(solve) inv <<- solve
    getSolve <- function() inv
    
    ## and return all four functions in a list
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
    
}


## cacheSolve:
## Checks to see if there is a cached inverse for x
## - if so, return that cached inverse matrix
## - if not, calculates the inverse using solve(), caches it and returns it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Look for a cached Inverse...
    inv <- x$getSolve()
    
    ## ...and if there is one, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Otherwise run the solve() function...
    data <- x$get()
    inv <- solve(data, ...)
    
    # ...cache it...
    x$setSolve(inv)
    
    ## ...and return the inverse
    inv
    
}