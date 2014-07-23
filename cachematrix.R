## makeCacheMatrix creates a special matrix object, and then cacheSolve
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmean <- function(jar) inv <<- jar
        getmean <- function() inv
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
        
}


## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.

cacheSolve <- function(x, ...) {
        bar <- x$getmean()
        if(!is.null(bar)) {
                message("getting cached data")
                return(bar)
        }
        data <- x$get()
        bar <- solve(data, ...)
        x$setmean(bar)
        bar
}

