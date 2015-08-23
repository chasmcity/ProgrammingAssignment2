## makeCacheMatrix() is first passed an invertible matrix, x

## i (for inverse) is initially set to NULL

## set() defines a function to set the value of matrix, x (from makeCacheMatrix), to y
## set() also resets the value of i to NULL incase we are overwriting a previous cache

## get() returns x from the parent environment, makeCacheMatrix()

## setInverse() can be passed a matrix, inverse, from cacheSolve() and sets the value
## of i in the parent environment to inverse

## getInverse() returns i from the parent environment
## if cacheSolve() has not been run, then getInverse() returns NULL

## makeCacheMatrix() then returns a list of the newly defined funtions set(), get(),
## setInverse(), and getInverse()

## ====================================================================================

## cacheSolve() is passed the list of functions, x, from makeCacheMatrix()

## the value of i is set to x$getInverse(), where the cached inverse may be stored

## if i != NULL, then cacheSolve() returns i as is and stops there

## otherwise, the function pulls the original matrix from x$get()
## then calculates the inverse and sets i to that value

## the new value of i is passed to x$setinverse()
## and cacheSolve() returns i

## ====================================================================================

## makeCacheMatrix() returns a list of functions that can cache an inverse of an
## input matrix, x

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve() computes the inverse of the input matrix and stores it in x$setInverse,
## only if the inverse has not been cached in the list, x, returned by makeCacheMatrix()
## otherwise, it returns the value of the inverse from the cache

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    else{
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        
        i
    }
}