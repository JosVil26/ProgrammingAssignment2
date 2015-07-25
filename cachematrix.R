## Put comments here that give an overall description of what your
## functions do
## My functions permit to calculate the inverse of a matrix by using a cache
## method

## Write a short comment describing this function
## This function creates a special vector wich contain operations to get and set
## the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function returns the inverse of 'x' by search in the cache and if isn't 
## found return the inverse by the solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
