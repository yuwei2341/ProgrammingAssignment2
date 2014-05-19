## Caching the Inverse of a Matrix

# makeCacheMatrix() creates a special "matrix", which is
# really a list containing a function to
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setInv <- function(inv) invX <<- inv
    getInv <- function() invX
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


# cacheSolve(x) calculates the inverse of the special "matrix" x, 
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it `get`s the inverse from the
# cache and skips the computation. Otherwise, it calculates the inverse of
# the data and sets the value of the inverse in the cache via the `setInv`
# function.
# Before computing the inverse, the function determines if x is a square matrix
# and return NULL if not; If x is square, then the function determines if x 
# is non-singular and return NULL if not. The latter needs the package matrixcalc.
# In case one doesn't install it, that part is commented

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## check if the inverse exists, return it if so
    invX <- x$getInv()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    data <- x$get()
    
    ## make sure it's square
    if (ncol(data) != nrow(data)) {
        message("The matrix must be square")
        return()
    }
    
    ## make sure the squre matrix is non-singular; need the package matrixcalc
#     if !is.non.singular.matrix(data) {
#         message("The square matrix must be non-singular")
#         return()
#     }
    
    ## Get the inverse and assign it to x
    invX <- solve(data, ...)
    x$setInv(invX)
    invX
}
