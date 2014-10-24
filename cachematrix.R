## Functions for caching the inverse of a matrix

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # setting the matrix value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # getting the matrix
        get <- function() x
        # setting the inverse matrix value
        setinv <- function(solve) m <<- solve
        # getting the inverse matrix
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix function
## return       an inverse matrix of input matrix x
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        # if the inverse for the current matrix has been already calculated, function returns the cache value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise it calculates the inverse matrix, sets the value in the cache and returns the value
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
