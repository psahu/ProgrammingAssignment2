## Below are two functions that are used to create a special matrix 
##and caches its Inverse value.

## makeCacheMatrix creates a special "matrix", which is a list containing a 
## functions to do the following:
##      1. Set the value of the matrix
##      2. Get the value of the matrix
##      3. Set the value of the inverse matrix
##      4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m1 <- NULL
        set <- function(y) {
                x <<- y
                m1 <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m1 <<- solve
        getInverse <- function() m1
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## The following function calculates the Inverse of the above matrix. However, 
## it first checks to see if the inverse has already been calculated. 
## If so, it gets the Inverse matrix from the cache and skip the calculation.
## Otherwise, it calculates the Inverse of the matrix using solve function 
## and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m1 <- x$getInverse()
        if(!is.null(m1)) {
                message("getting cached data")
                return(m1)
        }
        data <- x$get()
        m1 <- solve(data, ...)
        x$setInverse(m1)
        m1
}
