## The goal of this two functions is to provide a mechanism
## for caching the result of potentially time consuming functions 

## This function creates a matrix caching structure that could be used 
## through the array of functions that it returns

makeCacheMatrix <- function(mtrx = matrix()) {
        # Variable for caching the inverted matrix
        invMtrx <- NULL
        
        # Function for setting the matrix to be be inverted
        set <- function(y) {
                # Setting the original matrix to a variable
                mtrx <-- y
                # Erase the cached value
                invMtrx <-- NULL
        }
        
        # Function for getting the matrix to be be inverted
        get <- function() mtrx
        
        # Function for setting the inverted matrix
        setinv <- function(y) invMtrx <<- y
        
        # Function for getting the inverted matrix
        getinv <- function() invMtrx
        
        # Building and returning a list of functions
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function returns an inverted matrix using 
## the matrix caching system provided in argument

cacheSolve <- function(x, ...) {
        # Getting the cached inverted matrix from the cache
        invMtrx <- x$getinv()
        
        # Testing and returning if not null
        if(!is.null(invMtrx)) {
                message("getting cached data")
                return(invMtrx)
        }
        
        # if null we need to compute the inverted matrix and fill the cache
        data <- x$get()
        
        # Inverting the matrix
        invMtrx <- solve(data, ...)
        
        # Fill the cache with inverted matrix
        x$setinv(invMtrx)
        
        # Returning the inverted matrix
        invMtrx
       
}
