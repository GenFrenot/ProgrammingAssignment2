## These functions are to complete Week 2 of R Programming Certification 
## Goal:
##     store a matrix 'x' in cache
##     return a matrix that is the inverse of 'x'


## makeCacheMatrix stores a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y # caches a new matrix
        inv <<- NULL # clear the previously cached inverted matrix 
                     # as it would not match anymore
    }
    get <- function() x
    setInverted <- function(invertedM) inv <<- invertedM
    getInverted <- function() inv
    list(set = set, get = get,
         setInverted = setInverted,
         getInverted = getInverted)
}


## cacheSolve returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverted()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverted(inv)
    inv
    
}
