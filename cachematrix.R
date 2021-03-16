## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse
    i <- NULL
    
    ## Set the matrix and inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Set the inverse of the matrix.
    setInverse <- function(inverse) 
        i <<- inverse
    
    ## Get the inverse of the matrix
    getInverse <- function() 
        
        ## Return the inverse
        i
    
    ## Return a list containing the function to get and set the matrix and its 
    ## inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Calculates the inverse of a matrix. Uses 'makeCacheMatrix' to both set and get 
## any inverse and matrix, as well as checks if the inverse of the currently set 
## matrix has already been calculated, in which 'cacheSolve' will retrieve the 
## inverse from the cache instead.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    
    ## Condition that returns the inverse if it has already been set 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Gets the matrix
    data <- x$get()
    
    ## Calculates the inverse
    i <- solve(data)
    
    ## Sets the calculated inverse
    x$setInverse(i)
    
    ## Returns the inverse
    i
}
