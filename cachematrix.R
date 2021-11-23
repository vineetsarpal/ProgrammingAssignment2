## Put comments here that give an overall description of what your
## functions do

## Following functions let us create a matrix, solve for and cache its inverse
## If the inverse of that matrix is already computed
## the system doesn't need to compute it again and rather fetch it from cache

## Write a short comment describing this function
## Function to create a matrix and provide operations to set/get the inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Function to get the inverse of matrix and fetch it from cache if present

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
