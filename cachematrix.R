## This file contains the definition of two functions:
##
##	- "makeCacheMatrix" contains the definition of 
##	a matrix and internal functions manage its 
##	definition and its inverse.

##	- "cacheSolve" enable to cache the calculation 
##	of the inverse of a given matrix.


## Description:
##	Given a matrix, 'makeCacheMatrix' contains the 
##	definition of a numeric matrix and the following 
##	functions:
##	- "get", used to access the contents of the matrix.
##	- "set", used to define the contents of the matrix.
##	- "getinverse", used to access the inverse of the matrix.
##	- "setinverse", used to define the inverse of the matrix.
##
##	Parameters:
##	- "x", matrix whose management this function deals with.

makeCacheMatrix <- function(x = matrix()) {
	## Inverse matrix of matrix x:
        inv_matrix <- NULL
        
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }

        get <- function() x

        setinverse <- function(solve)  inv_matrix <<- solve

        getmean <- function() inv_matrix

        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Description:
##	Given a matrix, 'makeCacheMatrix' caches 
##	the calculation of a given matrix.
##
##	Parameters:
##	- "x", matrix whose inverse this function caches.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinverse()

        if (!is.null(inv_x)) {
                message("getting cached inverse matrix of x")
                return(inv_x)
        }

        # If execution flow didn't enter the previous 'if' block, then it is necessary to calculate the inverse matrix of x:
        inv_x <- solve(x)
        x$setinverse(inv_x)
        inv_x
}
