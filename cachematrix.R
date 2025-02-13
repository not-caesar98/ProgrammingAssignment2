# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize inverse as NULL
        
        # Function to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset inverse when new matrix is set
        }
        
        # Function to get the matrix
        get <- function() x
        
        # Function to set the inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # Function to get the inverse
        getInverse <- function() inv
        
        # Return a list containing the functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()  # Retrieve cached inverse
        
        # If the inverse is already cached, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Otherwise, compute the inverse, store it in cache, and return it
        data <- x$get()
        inv <- solve(data, ...)  # Compute the inverse
        x$setInverse(inv)  # Store inverse in cache
        inv
}
