## Caching the inverse of a matrix so we don't need to recompute it
## when we need it again.
## How to use:
## m: an invertible matrix
## cachem <- makeCacheMatrix(m)
## inverse <- cacheSolve(cachem)

## Create a list containing functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) inverse <<- inv
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of the 'Matrix' object created by makeCacheMatrix.
## It first check if the inverse is calculated, if so, it get the inverse
## from cache without calculation. Otherwise, it calculates the inverse of 
## the Matrix and store it in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
