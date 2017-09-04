## For the functions below, one of the caches the inverse of a matrix and the
## other function checks see if the inverse of the matrix is in the cache before
## it finds the inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    get <- function() x
    list(get_inverse = get_inverse, set_inverse = set_inverse, set = set, get = get)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x''
    
    inverse_mtrx <- x$get_inverse()
    if (!is.null(inverse_mtrx)) {
        message("getting cached data")
        return(inverse_mtrx)
    }
    
    # create inverse of matrix using the solve
    mtrx < x$get()
    inverse_mtrx <- solve(mtrx)
    x$set_inverse(inverse_mtrx)
    inverse_mtrx
}
