## These two functions calculate a matrix inverse and store it in a cached 
## list for later retrieval

## makeCacheMatrix creates a list that contains functions to set the value of a 
## matrix, get the value of a matrix, set the value of the matrix inverse, and 
## get the value of a matrix inverse. These are stored for later retrieval.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve either retrieves the inverse of a matrix from an already
## calculated cache or calculates it from scratch

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
