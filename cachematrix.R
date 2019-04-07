## These functions can be used to set the value of an invertible matrix,
## calculate and cache its inverse, then retreive the cached value.

## This function creates a special matrix that sets the value of the  
## matrix, gets the value of the matrix, sets the value of its inverse,
## and gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function () i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of a matrix if it is already
## cached or calculates the inverse of matrix.

cacheSolve <- function(x, ...) {
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
