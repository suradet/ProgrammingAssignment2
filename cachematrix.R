## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special "matrix", which is really a list containing
# a function to
# set : set the matrix
# get : get the matrix
# setinverse : set the inverse of the matrix
# getinverse : get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve retrieves the cached inverse of the matrix if available,
# otherwise calculates the inverse of the matrix and store it in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)
	m
}

# Test
# x <- matrix(1:4, nrow = 2, ncol = 2)
# y <- makeCacheMatrix(x)
# cacheSolve(y)
