### The following 2 functions are an adaptation of the example given
### to calculate the mean of a vector, using cached data when 
### available. Combined together, they compute the inverse of the 
### matrix given as an argument. If the inverse was computed earlier
### with the same functions, the operations takes less time only 
### cached data is returned.   


## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
    
    # Initialize the sored inverse matrix with NULL
    Inv <- NULL
    
    # The set function enables to change the stored (cached) matrix and 
    # re-initialize its cached inverse
    set <- function(y){
        x <<- y 
        Inv <<- NULL
    }
    
    # The get function returns the stored matrix
    get <- function() x
    
    # The setInverse function stores the 
    setInverse <- function(inverse) Inv <<- inverse
    
    # The getInverse function returns the stored (cached) inverse
    getInverse <- function() Inv
    
    # Return the special "matrix" object
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## Compute the inverse of the special "matrix"
cacheSolve <- function(x){
    
    # Retrieve the inverse from the cache if already computed
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
    # Else, compute the inverse, store it for future use 
    # then return it.
    Mat <- x$get()
    Inv <- solve(Mat)
    x$setInverse()
    Inv
}