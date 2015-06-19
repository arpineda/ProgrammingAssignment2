## The makeCacheMatrix function creates a list of four functions which 
## facilitates the storage of the matrix inverse of the input matrix (x) in the 
## cache to avoid unnecessary computation  

makeCacheMatrix <- function(x = matrix()) {
    # Creates a list of four functions which facilitates the storage of the
    # matrix inverse of the input matrix (x) in the cache to avoid unnecessary
    # computation.
    #
    # Args:
    #   x: invertible matrix.
    #
    # Returns:
    #   A list (object) with four functions defined with allow for setting the
    # value of the matrix and its inverse (get and getinv functions), setting 
    # the value of the matrix and its inverse (set and setinv functions).
    #
    # Sample Usage:
    # mDat <- matrix(c(2,0,0,1),nrow=2,ncol=2,byrow=TRUE)
    # mDatCache <- makeCacheMatrix(mDat)
    # mDatCache
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function takes in an object of type cacheMatrix and checks 
## whether the inverse has been computed already.  If it has, it returns the
## matrix inverse from the cache, if it hasn't it computes and returns the
## matrix inverse.  The function assumes the matrix is invertible.

cacheSolve <- function(x, ...) {
    ## The cacheSolve function takes in an object of type cacheMatrix and checks 
    ## whether the inverse has been computed already.  If it has, it returns the
    ## matrix inverse from the cache, if it hasn't it computes and returns the
    ## matrix inverse.  The function assumes the matrix is invertible.
    #
    # Args:
    #   x: invertible cacheMatrix.
    #
    # Returns:
    #   matrix that is the inverse of 'x'
    #
    # Sample Usage:
    # mDatInv <- cacheSolve(mDatCache)
    # mDatInv
    ## During the second call, the value is retrieved from the cache
    # mDatInv <- cacheSolve(mDatCache)
    # mDatInv
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    mat <- x$get()
    xinv <- solve(mat, ...)
    x$setinv(xinv)
    xinv
}
