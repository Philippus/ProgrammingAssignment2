## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than computing it repeatedly. These functions cache the inverse of a matrix.

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

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

## example of usage:
## > m <- matrix(c(1, 2, 3, 4, 5, -6, -7, -8, -9), 3, 3)
## > solve(m)
##             [,1]      [,2]        [,3]
## [1,] -1.29166667 1.0833333  0.04166667
## [2,] -0.08333333 0.1666667 -0.08333333
## [3,] -0.37500000 0.2500000 -0.04166667
## > n <- makeCacheMatrix(m)
## > cacheSolve(n)
##             [,1]      [,2]        [,3]
## [1,] -1.29166667 1.0833333  0.04166667
## [2,] -0.08333333 0.1666667 -0.08333333
## [3,] -0.37500000 0.2500000 -0.04166667
## > cacheSolve(n)
## getting cached data
##             [,1]      [,2]        [,3]
## [1,] -1.29166667 1.0833333  0.04166667
## [2,] -0.08333333 0.1666667 -0.08333333
## [3,] -0.37500000 0.2500000 -0.04166667
## >
