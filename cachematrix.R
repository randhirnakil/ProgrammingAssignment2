## Put comments here that give an overall description of what your
## functions do
## This function creates a cached matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        ## setter for the inverse function
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        ## getter for the inverse function        
        get <- function() x
        
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        
        list(get = get, set = set, setinv = setinv, getinv = getinv)
        
}


## Write a short comment describing this function
## This function returns cached inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        
        if(!is.null(i){
                message("getting cached data")
                return(i)
        }
        
        ## Calculating the inverse
        data <- x$get()
        i <- solve(data, ...)
        
        ## Cache the inverse
        x$setinv(i)
        
        ## Return the inverse
        i
}