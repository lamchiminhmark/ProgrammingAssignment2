## These 2 functions create a special "matrix" that can cache
## its inversion, and compute that inversion if it is not 
## already cached.

## This function returns a list of functions that are attributes
## of the special "matrix" that can cache its inverse. The
## functions include:
## 1. get: retrieving the matrix value.
## 2. set: assigning new matrix value.
## 3. getInverse: retrieving the inverse.
## 4. setInverse: assigning new inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() {x}
        
        setInverse <- function(value) {inverse <<- value}
        
        getInverse <- function() {inverse}
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix". It first 
## checks to see if the inverse has been cached. If yes, it will pull
## the results from the cache and skip the calculations, otherwise, it
## will do the calculation.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setInverse(inverse)
        inverse
}
