## The functions below provide the means to avoid repeated computation
## of the inverse of a square matrix, if the matrix did not
## change.
## Limitation: The functions do not check if the given matrix is
## square and invertible and will respond with a solve Error
## if the matrix cannot be inverted.


## This function creates a matrix object. It stores a matrix and
## the cached value of its inverse.
## Additional Functions are provided to read and manipulate the
## stored values.

makeCacheMatrix <- function(x = matrix()) {
		# initialize cache variable with NULL
        s <- NULL
        # set function: replaces the existing matrix with a new
        # one and resets the cache variable
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        # get function: returns the matrix
        get <- function() x
        # setsolve function: stores the inverse matrix in the
        # cache variable
        setsolve <- function(solve) s <<- solve
        # getsolve function: returns the cached inverse matrix
        getsolve <- function() s
        # create and return the matrix object as a list.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of a matrix object created by
## makeCacheMatrix(x)

cacheSolve <- function(x, ...) {
        # read the cached inverse matrix from 'x'
        s <- x$getsolve()
        # if the cache is not empty (not NULL), return the cached value
        # and print a message 
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        # else get the matrix from 'x', compute the inverse,
        # store the result in the cache variable of 'x' and return
        # the inverse
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s        
}

## Note to me: matrix(c(9,3,5, -6,-9,7, -1,-8,5), nrow=3, ncol=3)
## is square and invertible
