## This R file creates a special "matrix" object that can cache its inverse.
##
## Computing the inverse of a square matrix can be done with the solve() function in R. 
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##
## Note: We assume that the matrix supplied is always invertible
##
## Brian Buckley September 2014


## makeCacheMatrix wraps an input matrix 'x' with an object that also contains
## the inverse of 'x' plus a list of mutator functions that cacheSolve uses

makeCacheMatrix <- function(x = matrix()) {
    	# Initialize the cached inverse matrix value to NULL
    	inv <- NULL
    
    	# Set the value of the input matrix to a new value 'y'
    	# Since the input matrix has now changed we re-initialize our
    	# cache of the inverse to NULL again to keep the input and inverse
    	# matrices aligned
    	set <- function(y) {
    	    x <<- y
    	    inv <<- NULL
    	}
    
    	# Get the value of the input matrix
    	get <- function() x
    	
    	# Set the cache of the inverse of the matrix x calculated inside cacheSolve
    	setinv <- function(solve) inv <<- solve
    
    	# Get the value of the inverse matrix from our cache
    	getinv <- function() inv
    
    	# This is the list of functions for makeCacheMatrix()
    	list(set = set, get = get,
    	     setinv = setinv,
    	     getinv = getinv)
}


## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    	# Check if we already have a value for the inverse of x in the cache
    	# by calling the getinv() function from makeCacheMatrix on our special input
    	# matrix x
    	inv <- x$getinv()
    	if(!is.null(inv)) {
        	message("returning cached inverse matrix")
        	return(inv)
	}

	# If we have no cached value we will use R's solve() to calculate the value
	# and we will store the value in the cache prior to returning by calling
	# the setinv() function from makeCacheMatrix
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)

	message("returning calculated inverse matrix - now cached")
	inv
}
