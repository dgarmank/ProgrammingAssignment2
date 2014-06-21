# Programming Assignment 2 for Coursera Intro to R Programming, from JHU.
#
# Two functions are presented to optimize matrix calculations, by initially
# computing a matrix inverse and storing it into a cache variable, and
# retrieving it subsequently, without re-computing.
# Example usage:
#
#   construct the object AND matrix this way
#       x <- makeCacheMatrix(matrix(c(1:4),nrow=2,ncol=2))
#   OR construct the object and initialize the matrix this way
#       x <- makeCacheMatrix()
#       x$set(matrix(c(1:4),nrow=2,ncol=2))
#   display the matrix
#       x$get()
#   Return inverse; no msg to indicate calc of inverse is returned
#   It invokes x$setInv() and x$getInv()
#       cacheSolve(x)
#   Return inverse again; this time, msg indicates return of a cached inverse
#   It only invokes x$getInv
#       cacheSolve(x)

# Function makeCacheMatrix(x) creates an object that takes a matrix x and
# returns a list of four function methods on the object (not the matrix).
# These methods are:
#   1.  set the value of the matrix
#   2.	get the value of the matrix
#   3.	set the value of the matrix inverse
#   4.	get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                 # set matrix inverse not computed
    
    set <- function(y) {
        x   <<- y               # store the matrix
        inv <<- NULL            # set matrix inverse not computed
    }
    
    get <- function() x         # return the matrix
    
    setInv <- function(result) inv <<- result   # store the matrix inverse
    
    getInv <- function() inv    # simply ret the current matrix inverse value
    
    list(set = set, get = get,  # return the list of method/function names
         setInv = setInv,
         getInv = getInv)
}

# Function cacheSolve(x) first checks for a cached inverse on the object
# x, for a previously computed (non-NULL) matrix inverse, and returns it
# along with a message indicating that the cache var is being returned.
# If otherwise NULL, the matrix inverse of x is computed, stored in the cache
# var, and returned.

cacheSolve <- function(x, ...) {
    
    myinv <- x$getInv()                 # retrieve cached matrix inverse
    
    if(!is.null(myinv)) {
        message("getting cached data")  # indicate no calculation performed
        return(myinv)                   # and return the value
    }
    
    mymtrx <- x$get()                   # get the matrix
    myinv  <- solve(mymtrx)             # compute the matrix inverse
    x$setInv(myinv)                     # store into the cache var
    myinv                               # and return the matrix inverse
}
