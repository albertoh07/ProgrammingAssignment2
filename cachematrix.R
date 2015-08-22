## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix uses scoping rules and stores matrices in memory

makeCacheMatrix <- function(x = matrix()) {
    # invert will store the cached inverse matrix
    inv <- NULL
    # Setter for matrix
    set <- function(y) {
        x <<- y
        invert <<- NULL
    }
    # Getter for matrix
    get <- function() x
    
    # Setter and Getter for the inverse
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    # Return the matrix with defined functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    
    ## If the inverse is calculated, return it. 
    if(!is.null(inv)){
        message("Getting chached data.")
        return(inv)
    }
    
    ## Otherwise, calculate it
    message("The inverse is not in memory. We need to calculate it.")
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Cache inverse...
    x$setinverse(inv)
    
    inv
}
