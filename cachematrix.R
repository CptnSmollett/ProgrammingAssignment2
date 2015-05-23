## This is a code file for programming assignment 2 in R Programming course
## by Dr. Peng.
## These functions inverse a matrix with caching as requested in original asssignment.


## makeCacheMatrix(x)
## This function returns a list of basic operations for cached matrix inversion.
## It takes a matrix as a single argument. 
## Arguments:
##      x - matrix for inversion, default value is empty matrix
## Output list elements:
##      set - function taking a matrix as a single possible argument.
##            Other types are not coerced to matrix class.
##            A check for matrix class and square matrix is performed.
##            Both tests return warning message in case of failure.
##            This function drops cached inversed matrix value when used.
##            Used to cache a matrix value for further inversion.
##      get - funtion with no arguments returns matrix value from cache.
##      setInversed - function taking a matrix as a single possible argument.
##                    A check for matrix class and square matrix is performed.
##                    Both tests return warning message in case of failure.
##                    Used to cache an inversed matrix value.
##      getInversed - funtion with no arguments returns inversed matrix value
##                    from cache.


makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    
    set <- function(y) {
        if (!is.matrix(y)) {
            warning("Argument passed is not a matrix",
                    immediate. = TRUE)
        }
        else if (ncol(y) != nrow(y)) {
            warning("Argument passed is not a square matrix",
                    immediate. = TRUE)
        }
        
        x <<- y
        inversed <<- NULL
    }
    
    get <- function() x
    
    setInversed <- function(i) {
        if (!is.matrix(i)) {
            warning("Argument passed is not a matrix",
                    immediate. = TRUE)
        }
        else if (ncol(i) != nrow(i)) {
            warning("Argument passed is not a square matrix",
                    immediate. = TRUE)
        }
        
        inversed <<- i
    }
    
    getInversed <- function() inversed
    
    list(get = get, set = set, setInversed = setInversed, getInversed = getInversed)
}


## cacheSolve(x, ...)
## Function taking a matrix value to inverse it using cache.
## If cache contains any inversed matrix value it is returned. Otherwise it is
## calculated and cached.
## Arguments:
##      x - list returned by makeCacheMatrix() function
##      ... - arguments passed to solve() function

cacheSolve <- function(x, ...) {
    # trying cache first
    i <- x$getInversed()
    if (!is.null(i)) {
        message("Using cached value")
        return(i)
    }
    
    # bad luck, no cached value available thus need to compute it in an honest way
    matrix <- x$get()
    i <- solve(matrix)
    x$setInversed(i)
    i
}
