
# These functions take a matrix, calculate its inverse using the solve() function, 
# and cache it so that if the same matrix is solved again, the cached data will be used.
# The first function, makeCacheMatrix, creates a list with the components of the function and environment addresses.
# After creating an object with the makeCacheMatrix, the user can add a matrix to the object using the set command.
# Use like this:
# > mycache <- makeCacheMatrix()
# > mycache$set(your matrix here)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# This function checks whether the inverse has already been solved.
# If it has, it returns the result from the cache with the message.
# If it has not, it solves the inverse of the matrix and displays it.
# Use like this:
# > cacheSolve(mycache)

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}
