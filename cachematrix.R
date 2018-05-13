## The two functions below show how R caches the inverse of a matrix,
## instead of a possible recomputation which takes time and effort in a data scientist.

## Function below creates a special matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- solve(x)
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function below computes the inverse of the special matrix returned by the makeCacheMatrix.
## The output is the inverse of the matrix and checks if it has been stored in the cache
## If the value has been cached, it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'inv'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}