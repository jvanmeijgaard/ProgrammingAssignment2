## Return the inverse of a matrix. Use caching to reduce compute time when 
## calculating the inverse multiple times.
##
## Usage:
## First use makeCacheMatrix to create a list object using the matrix to be inverted
## then use cacheSolve to obtain the inverse the matrix


## The function makeCacheMatrix creates a list of functions to 
## (1) set the value of the matrix, (2) get the value of the matrix
## (3) set the value of the inverse of the matrix, and (4) get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
    Minv <- NULL
    set <- function(y) {
        x <<- y
        Minv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) Minv <<- inv
    getinv <- function() Minv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve returns the inverse of the matrix using makeCacheMatrix
## If the invese has previously been calculated the cached value will be returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Minv <- x$getinv()
    if(!is.null(Minv)) {
        message("getting cached data")
        return(Minv)
    }
    data <- x$get()
    Minv <- solve(data)
    x$setinv(Minv)
    Minv
}


