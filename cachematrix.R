## makeCacheMatrix creates a function environment with the following functions:
# set - used to re-use the object by changing x and resetting cached inverse
# getmatrix - returns the matrix x stored in the function environment
# setinv - changes value of inv in function environemnt to value passed to it
# getinv - fetches the value of inv from the function environment
# also contains the variables x (the matrix) and inv (initially null)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, getmatrix = getmatrix,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve checks if a value has been assigned to inv in the makeCacheMatrix environment
# if so it simply fetches it
# if not it fetches the matrix, calculates the inverse and returns this.....
# but first it assigns this value to inv in the makeCacheMatrix environment
# so that next time it will simply fetch it without needing to calculate.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse:")
        return(inv)
    }
    data <- x$getmatrix()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}