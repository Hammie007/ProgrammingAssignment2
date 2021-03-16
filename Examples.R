## a function pair the cache the mean

## Create a numeric object (vector) that can cache a mean
makeVector <- function(x = numeric()) {
    
    ## Initialize the mean
    m <- NULL
    
    ## Set the numeric object and mean
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## Get the object and return the numeric object 'x'
    get <- function() 
        x
    
    ## Set the mean of the object
    setmean <- function(mean) 
        m <<- mean
    
    ## Get the mean of the object
    getmean <- function() 
        
        ## Return the mean
        m
    
    ## Return a list containing the function to get and set a vector and mean
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

## The following function calculates the mean of the special "vector" created 
## with the above function. However, it first checks to see if the mean has 
## already been calculated. If so, it gets the mean from the cache and skips the 
## computation. Otherwise, it calculates the mean of the data and sets the value 
## of the mean in the cache via the setmean function.
cacheMean <- function(x, ...) {
    
    ## Return the mean of the numeric object 'x'
    m <- x$getmean()
    
    ## Condition that returns the mean if it has already been set 
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Gets the numeric object
    data <- x$get()
    
    ## Calculates the mean
    m <- mean(data, ...)
    
    ## Sets the calculated mean in the numeric object
    x$setmean(m)
    
    ## Returns the mean
    m
}