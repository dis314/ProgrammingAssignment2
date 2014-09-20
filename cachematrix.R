## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 

## The functions below inverses a matrix and caches the result

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL	## clearing variable
        }
        get <- function() x
        setinv <- function(solve) 
        s <<- solve ## caching inverted matrix	
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
	  
	  ## checking for non-empty cache

        if(!is.null(s)) {
                message("getting cached data")
                return(s) ## return of cached data
        }
        data <- x$get()
        s <- solve(data)
        x$setinv(s)
        s
}
