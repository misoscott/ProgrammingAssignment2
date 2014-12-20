# This is programming assignment 2 for JH/Coursera R programming course
# for this assignment, we have to write the following function:
#
#  makeCacheMatrix: This function creates a special "matrix" object that can
#                   cache its inverse.
#
#  cacheSolve: This function computes the inverse of the special "matrix" 
#              returned by makeCacheMatrix above. If the inverse has already 
#              been calculated (and the matrix has not changed), then 
#              cacheSolve should retrieve the inverse from the cache.


#
# The makeCacheMatrix() function creates an object containing a
# matrix. This object can be used to cache/return the inverse of
# the stored matrix. You can test this function with the following call:
#
# m<-makeCacheMatrix(matrix(c(-1,-2,1,1), nrow=2, ncol=2)) 
#
makeCacheMatrix <- function(x = matrix()) 
{
    # initialize saved inverse to NULL 
    inverse <- NULL
    
    # here's the set() function for the matrix, it 
    # saves the matrix in variable x
    set <- function(y) 
    {
        x <<- y
        inverse <<- NULL
    }
    
    # here's the get() function, it returns the current matrix 
    get <- function() x
    
    # the setinverse() function assigns the passed value to the "inverse"
    # inverse variable (member?) of the makeCacheMatrix object
    setinverse <- function(value) inverse <<- value
    
    # getinverse() returns the currently cached value (which might be NULL)
    getinverse <- function() inverse
    
    # create the list value returned by makeCacheMatrix()
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
}


# The cacheSolve() function will compute the inverse of a matrix, or return
# a previously computed (and cached) inverse.
#
# Inputs
#   x: cache matrix object created by makeCacheMatrix()
#
# Outputs
#   inverse: the matrix inverse
#
# Assumptions
#   x contains an invertible matrix
#
# Testing
#   assuming you used m<-makeCacheMatrix(matrix(c(-1,-2,1,1), nrow=2, ncol=2)) to
#   create m, you can test this function with "cacheSolve(m)"
cacheSolve <- function(x, ...) 
{
    # Return a matrix that is the inverse of the passed matrix. First, check to see
    # if we previously computed (and saved) the inverse. If so, return
    # the stored value immediately. Otherwise, compute, save, and return
    # the inverse of x
    
    # have we been here before?
    inverse <- x$getinverse()
    if( !is.null(inverse)) 
    {
        # yes, we have, so return the cached value.
        message("returning cached value.")
        return(inverse)
    }

    # if we get here, we need to compute/save/return the inverse

    # first, get matrix from x
    matrix <- x$get()
    
    # compute inverse
    inverse <- solve(matrix)

    # save inverse in x
    x$setinverse(inverse)

    # finally, return the inverse
    return(inverse)
}
