## These functions cache and invert a matrix, respectively. The second function
## only solves the matrix if it is not already cached

## This function is designed to cache the inverse of a matrix object
makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        
        #set function
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        
        #get function
        get <- function() x
        
        #set inverse function
        setminv <- function(solve) minv <<- solve
        
        #get inverse function
        getminv <- function() minv
        list(set = set, get = get,
             setminv = setminv,
             getminv = getminv)
        }

## This function is designed to compute the inverse of a matrix object,
## using a cached version instead if it's available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getminv()
        if(!is.null(minv)) {
                message("cached data being retrieved")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setminv(minv)
        minv
}
