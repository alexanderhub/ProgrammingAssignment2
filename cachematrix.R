## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {    
        mymatrix <- NULL 
        
        set <- function(y) {
                x <<- y
                mymatrix <<- NULL 
        }
        get <- function() x                                     ## return the input matrix
        setInv <- function(inverse) mymatrix <<- inverse        ## set the inversed matrix
        getInv <- function() mymatrix                           ## return the inversed matrix
        
        ## return a list that contains these functions
        list(set = set, get = get, setInv = setInv, getInv = getInv) 
                                                        ## examples of makeCacheMatrix use:
                                                        ## x <- makeCacheMatrix(matrix1)
                                                        ## x$set(matrix2) # to change matrix
                                                        ## x$get # to get matrix
                                                        ## x$setInv # to set the inverse
                                                        ## x$getInv # to get the inverse
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        invmatrix <- x$getInv() 
        if(!is.null(invmatrix)) { 
                message("Getting cached data, please ba patient...")
                return(invmatrix) 
        }
        ## if the inverse has not already been calculated,
        data <- x$get()                 ## get the matrix object
        invmatrix <- solve(data)        ## solve it
        x$setInv(invmatrix)             ## set the inverse to the object
        invmatrix                       ## return the inverse
}