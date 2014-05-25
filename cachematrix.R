## This code creates a cached matrix and stores its inverse in order to not repeat the inverse
## calculus each time.



## This function creates a special object "matrix" and stores its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # the inverse variable
    inverse <- NULL
    
    ## set the matrix function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the matrix function
    get <- function() x
    
    
    ## set the inverse fucntion, stores the inverse
    setInverse <- function(i) inverse <<- i
    
    ## get the inverse function
    getInverse <- function() inverse
    
    ## generation of the special object with the appropriate methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## this function calculates the inverse of a matrix
## retrives the cached inverse if it exists
cacheSolve <- function(x, ...) {
        
        ## get the inverse
        inverse <- x$getInverse()
        
        ## if the inverse of the matrix is cached
        if(!is.null(inverse)){
            
            message("getting cached inverse")
            ## return cached inverse
            return(inverse)
            
        }
        
        ## else if no inverse is cached
        ## get the matrix
        matrix <- x$get()
        ## solve the inverse of the matrix
        inverse <- solve(matrix)
        ## store the inverse in cache
        x$setInverse(inverse)
        
        ## return inverse
        inverse
}
