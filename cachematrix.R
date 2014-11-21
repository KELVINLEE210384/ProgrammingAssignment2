## A pair of functions to create a matrix and cache its inverse

## A nested function that creates a matrix and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Initialized the inverse of 'x' as an empty variable 'i'
    i <- NULL
    
    ## Function "set" to create a matrix that replaces x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Function "get" to return 'x'
    get <- function() x
    
    ## Function "setInverse" to set the inverse matrix to i
    setInverse <- function(inverse) i <<- inverse
    
    ## Function "getInverse" to return i
    getInverse <- function() i
    
    ## A list of functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## A nested function to solve for the inverse of the matrix returned by
## "makeCacheMatrix" function above. If the inverse is already computed, the 
## "cacheSolve" will retrieve the inverse from the cache, else compute the inverse 
## and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()

    ## If the inverse of 'x' is present, message "getting cached data" and return i
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    ## If 'i' is NULL, get 'x'
    data <- x$get()
    ## Solve for the inverse of 'x'
    i <- solve(data)
    ## Set the solved inverse of 'm' to 'i'
    x$setInverse(i)
    ## Return i
    i
}