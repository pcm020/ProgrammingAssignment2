## Functions to calculate the inverse of a matrix and cache the result
## Assignment for Coursera R Programming course. 2014-04-24

## Function to create a matrix with its cacheable inverse matrix over a list.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    
    # Set de matrix function
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # Get the matrix function
    get <- function() x
    
    # Set the calculate inverse matrix
    setsolve <- function(solve) s <<- solve
    
    # Get the inverse matrix
    getsolve <- function() s
    
    # Return a list with the functions
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Function to calculate and cache the inverse (solve) of a matrix

cacheSolve <- function(x, ...) {
    # Get cached inverse matrix
    s <- x$getsolve()
    
    # If it was calculated before, use it.
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # If wasn't calculate, get the matrix
    data <- x$get()
    
    # Calculate de inverse matrix with solve function
    s <- solve(data, ...)
    
    # Save the inverse matrix
    x$setsolve(s)
    
    ## Return a matrix that is the inverse of 'x'
    s
}
