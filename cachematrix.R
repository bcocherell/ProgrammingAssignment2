## Assignment: Caching the Inverse of a Matrix
##
## These two functions (when used together) allow the user to cache the inverse 
## of a matrix which is a potentially time-consuming computation.
##
## If the contents of a matrix do not change, it may make sense to cache the value 
## of the inverse so if needed again, it can be looked up in the cache rather than be recomputed.
## 
## Example usage:
## 
## > a <- makeCacheMatrix(matrix(c(4,2,7,6), nrow = 2, ncol = 2))
##
## > a$get()
##      [,1] [,2]
## [1,]    4    7
## [2,]    2    6
##
## > cacheSolve(a)
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
##
## > cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4


## This first function, makeCacheMatrix creates a special "matrix", which is really a list containing functions to...
## - Set the value of the matrix
## - Get the value of the matrix 
## - Set the value of the inverse
## - Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y = matrix()) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinv <- function(inv = matrix()) i <<- inv
	getinv <- function() i
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## The following function, cacheSolve computes the inverse of the special "matrix" created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache 
## and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the 
## cache via the setinv function.
##
## Note: It is assumed that the matrix supplied (x) is always a square invertible matrix

cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	i
}