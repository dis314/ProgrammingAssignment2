## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 

## The functions below inverses a matrix and caches the result

## This function makes matrix, inverse it and cache the result

makeCacheMatrix <- function(x = matrix()) {
	  s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve	
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function checks if matrix is already inverted and return the cached result.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setinv(s)
        s
}
