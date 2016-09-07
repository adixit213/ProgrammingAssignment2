# Caching the inverse of a matrix

# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

# library(MASS) Using this library to call the ginv() function, as it calculates generalized inverse of a matrix "x"

makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
        set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) mat_inv <<- inverse
    getinv <- function() mat_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data using the Solve() function. 
# Sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
    mat_inv <- x$getinv()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
        data <- x$get()
        mat_inv <- solve(data, ...) # It seems that the solve() works for square matrices only 
        # mat_inv <- ginv(data) # Therefore also inculded the ginv() for a non-square matrix 
        x$setinv(mat_inv)  
        mat_inv ## Return a matrix that is the inverse of 'x'
}
