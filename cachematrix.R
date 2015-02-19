## Below two functions are used to create a special 
## object that stores a square invertible matrix and cache's its mean.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## and cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

## This function(makeCacheMatrix) returns a special "matrix" object that is really 
## a list containing four functions to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the Matrix inverse
##  4. get the value of the Matrix inverse
makeCacheMatrix <- function(x = matrix()) {
        ## returns the special "matrix" object
        
        ## intialize empty object to inverse matrix
        i <- NULL
        ## This function(set) sets the value of matrix
        set <- function(y) {
                ## store the new value of matrix in x
                x <<- y
                ## reset the inverse as value of matrix is changed
                i <<- NULL
        }
        ## This function(get) returns the value of matrix
        get <- function() x
        ## This function(setinverse) stores(caches) the values of inverse matrix 
        ## that is sent to it as an argument
        setinverse <- function(inverse) i <<- inverse
        ## This function(getinverse) returns the inverse of matrix
        getinverse <- function() i
        ## return the list of four functions set,get,setinverse and getinverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)        
}


## This function(cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above.If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrive the cached/already caculated value of inverse
        i <- x$getinverse()
        ## if cached value is found then display the message and return the inverse
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## if cached value is not found thet get the value of matrix 'x' 
        data <- x$get()
        ## calculate the inverse matrix 'i' using solve function 
        i <- solve(data, ...)
        ## store(cache) the value of inverse matrix 'i'
        x$setinverse(i)
        ## return inverse matrix 'i'
        i
}
