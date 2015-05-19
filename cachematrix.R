## This function creates a special "matrix" object that can cache its inverse.
## Part of the pair of functions to cache the inverse of a matrix

## Uses the name of makeCacheMatrix as required

makeCacheMatrix <- function(mtx = matrix()) {

        ## Initialize the INVERSE Matrix 
        inverse <- NULL
        
        ## SET the Matrix 
        set <- function(x) {
                mtx <<- x;
                inverse <<- NULL;
        }

        ## RETURN the matrix value 
        get <- function() return(mtx);

        ## SETING the inverse matrix
        setinv <- function(inv) inverse <<- inv;

        ## RETURN the inverse matrix 
        getinv <- function() return(inverse);

        return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Geting the value of Inverse value of the matrix
        inv <- x$getinv()
        
        ## Return the value of inverse if it is already cached 
	 if(!is.null(inv)) {
                message("Inverse Cached")
                return(inv)
        }
        
        ## If the inverse is not already calculated, get the matrix and caluclate the inverse
        data <- x$get()
        m <- solve(data) ## Calculate Inverse
        x$setinv(m) ## Cache the value of Inverse
        m ## retunn the inverse matrix 
}
