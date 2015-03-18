## These 2 functions calculate and cache the inverse of a matrix, so it does 
## not have to be calculated again, if needed.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {	
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks to see if there's a cached inverse of a matrix. If not
## it computes the inverse of the special "matrix" returned 
## by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
	  ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
