# makeCacheMatrix example
# type a <- makeCacheMatrix()
# type a$set(matrix(sample.int(16), 4, 4)). To invert the matrix, we must use a quadratic matrix
# type a$get() to see the quadratic matrix

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

# cacheSolve example
# type cacheSolve(a) to invert the matrix assigned to the "a" variable that we used in the function makeCacheMatrix()
# type again cacheSolve(a) to invert the matrix. But this time the solve() function will use the matrix stored in memory (m variable). This way is faster than recalculate the invert matrix from scratch.

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
}