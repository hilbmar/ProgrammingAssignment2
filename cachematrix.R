## These functions will allow a matrix to be entered, cached, and the inverse of
## the matrix to be calculated.

## This function creates a "vector" that allows you to set and get the value of
## the matrix and to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i<<- solve
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will retrieve a previously calculated inverse of a matrix or
## calculate the inverse of a matrix if this has not yet been done using the
## information from the previous function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
