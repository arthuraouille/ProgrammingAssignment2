## These functions help create special matrices that can cache their inverse
## and use this cache to reduce computional costs when trying to 
## repeatedly calculate the inverse of the matrix.

## This function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # Set the inverse to NULL
    s <- NULL
    
    # Creates the "special" matrix with cacheable inverse
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    # Prints the "special" matrix
    get <- function() x
    
    # Caches the inverse of the "special" matrix
    setsolve <- function(solve) s <<- solve
    
    # Prints the inverse of the "special" matrix (or NULL if not yet cached)
    getsolve <- function() s
    
    # Return the list containing all the functions to set/get 
    # the "special" matrix and its inverse
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of a matrix, by checking 
## whether said matrix has a cached version of it or calculating it otherwise.

cacheSolve <- function(x, ...) {
    
    # Grabs inverse from the "special" matrix cache
    s <- x$getsolve()
    
    # Returns the cached inverse - with specific message, if found
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    # Creates a reference to the "special" matrix, if not found
    data <- x$get()
    
    # Calculates its inverse
    s <- solve(data, ...)
    
    # Caches the inverse within the "special" matrix
    x$setsolve(s)
    
    # Prints the calculated inverse
    s
}
