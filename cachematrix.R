## Examples
make_vector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(
                set = set, get = get,
                setmean = setmean,
                getmean = getmean
        )
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## Assignments

makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its inverse.
        inv <- NULL

        ## Function to set the matrix
        set <- function(matrix) {
                x <<- matrix
                inv <<- NULL ## Reset the cached inverse
        }

        ## Function to get the matrix
        get <- function() x

        ## Function to set the cached inverse
        setInverse <- function(inverse) {
                inv <<- inverse
        }

        ## Function to get the cached inverse
        getInverse <- function() inv

        ## Return a list of functions
        list(
                set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
        ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

        ## Computing the inverse of a square matrix can be done with the solve function in R.
        ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
        ## For this assignment, assume that the matrix supplied is always invertible.
        inv <- x$getInverse() ## Getting the inverse of the matrix object
        if (!is.null(inv)) { ## If the inverse is not NULL return it.
                message("Getting cached inverse")
                return(inv)
        }
        ## If it is null get the matrix and inverse it.
        mat <- x$get()
        inv <- solve(mat, ...)

        x$setInverse(inv) ## Cache the inverse now that we calculated it.

        inv
}
