## This is a submission for R Programming Assignment 2.
## Indentation in this file is set to 8.




## This makeCasheMatrix function creates a special "matrix" object that can cache its inverse.

makeCasheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {				## define function set
                x <<- y
                i <<- NULL
        }
  
        get <- function() x				## define function get
        setinverse <- function(solve) i <<- solve	## define function setinverse
        getinverse <- function() i			## define function getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

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