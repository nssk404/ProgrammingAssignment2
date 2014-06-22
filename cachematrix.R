# Creates a matrix that can store the value of its inverse. 
# Use the MakeCacheMatrix function to create a matrix for which
# the inverse can be stored. 
# eg: x <- makeCacheMatrix(matrix(c(2,2,3,2), 2, 2)) 
#
# Instead of calling solve(matrix(c(2,2,3,2), 2, 2)) 
# you can call cacheSolve(x)
# This will calculate the matrix inverse the first time, but 
# return the cached value every other time till the matrix 
# has been changed. 

## Creates a matrix whose inverse can be stored
# To Create a new matrix: 
#   x <- makeCacheMatrix(matrix(c(2,2,3,2), 2, 2))
# To get the value of the stored matrix:
#   x$get
# To store a new matrix
#   x$set(matrix(c(2,2,3,2), 2, 2))
# To get the stored inverse
#   x$getInverse()
# To store a new inverse
#   x$setInverse(newInvVal)
makeCacheMatrix <- function(x = matrix()) {
    m_inverse <- NULL
    set <- function(newMatrix){
        x <<- newMatrix
        m_inverse <<- NULL
    }
    get <- function(){
        x
    }
    getInverse <- function(){
        m_inverse
    }
    setInverse <- function(newInverse){
        m_inverse <<- newInverse
    }
    list(
        set = set,
        get = get,
        getInverse = getInverse,
        setInverse = setInverse
        )
}


# Looks in x for the cached Inverse value.
# If it exists it returns it, else it calculates
# the inverse. The inverse is then stored in x
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    storedInverse <- x$getInverse()
    if (!is.null(storedInverse)){
        message("Getting cached Inverse")
        return (storedInverse)
    }
    matrixToInvert <- x$get()
    invertedMatrix <- solve(matrixToInvert, ...)
    x$setInverse(invertedMatrix)
    invertedMatrix
}