#These functions are designed to work together to cache the inverse of a matrix. 
#makeCacheMatrix creates a special "matrix" object that can store a matrix and its inverse, while cacheSolve computes the inverse of the special "matrix". 
#If the inverse has already been computed and the matrix has not changed, cacheSolve will retrieve the cached inverse to avoid redundant calculations.


#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse property
    
    set <- function(y) {
        x <<- y   # Set the matrix value
        inv <<- NULL  # Reset the inverse when the matrix is set
    }
    
    get <- function() x  # Get the matrix value
    
    setInverse <- function(inverse) inv <<- inverse  # Set the cached inverse
    
    getInverse <- function() inv  # Get the cached inverse
    
    # Return a list of methods to interact with the matrix and its inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Get the cached inverse, if it exists
    
    if(!is.null(inv)) {  # If the inverse is already cached
        message("getting cached data")  # Print message indicating cached data usage
        return(inv)  # Return the cached inverse
    }
    
    data <- x$get()  # Get the matrix
    inv <- solve(data, ...)  # Compute the inverse of the matrix
    x$setInverse(inv)  # Cache the inverse
    inv  # Return the computed inverse
}

