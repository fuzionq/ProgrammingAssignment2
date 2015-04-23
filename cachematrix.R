## This function takes a matrix that can be inverted as its input.
## makeCacheMatrix returns a list of functions, that will then be used to set and get the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setI <- function(solve) m <<- solve
    getI <- function() m
    list(set = set, get = get,
         setI = setI,
         getI = getI)
}


## cacheSolve looks to see if the inverse of the matrix has been computed already via getI().
## If it has, it simply returns it. Otherwise, it solves it using the solve function and stores it via setI().

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getI()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setI(m)
    m
}
