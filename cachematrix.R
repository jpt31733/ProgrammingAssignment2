## The functions "makeCacheMatrix" and "cacheSolve" implement a
## cached version of matrix inversion.
## Usage : a) Given a matrix A, c <- makeCacheMatrix(A) creates a
##      cached version of A and assigns the functons for managing the cache to c.
##         b) cacheSolve(c) uses the functions 'stored' in c to retrieve the
##      inverse of the cached matrix.

## "makeCacheMatrix' creates a "matrix" that can cache its inverse.
## It keeps both the matrix and its inverse in the global environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(mat) {
                x <<- mat
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)        
}


## 'cacheSolve' calculates the inverse of a "cached matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
