## mackeCacheMatrix creates and returns
## a list of subfunctions to be used for caching.
## In these subfunctions the special assignement operator
## is used to store the matrix and the inverted matrix
## in the environment associated with mackeCacheMatrix.
## cacheSolve calls makeCacheMatrix subfunctions and 
## can therefore access the stored values for the matrix 
## and inverted matrix.
## ----------------------------------
## tutorial how to use subfunctions (for  caching mean of vector)
## https://asitarrives.wordpress.com/2014/10/18/understanding-lexical-scoping-in-r-great-guidance-for-community-ta-in-coursera/comment-page-1/#comment-12
## ----------------------------------

## subfunctions to store and manipulate matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setmean <- function(inverted) inv <<- inverted
        getmean <- function() inv
        list(set = set, get = get, setmean = setmean, getmean = getmean)
}



## Reads out the stored inverted matrix if it exists and 
## returns it.
## Computes the inveted matrix and stores it if necessary.

cacheSolve <- function(x, ...) {
        inv <- x$getmean()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(inv)
        inv
}