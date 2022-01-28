# 27-January-2022
# anhtn35
# Programming Assignment 2 in Week 03 of the Course "R-Programming"

## The function obtains a matrix for caching in order to find the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	u <- NULL 
	
	# I initialize here u as null
	for_s <- function(matr) {
		x <<- matr	# the value of the matrix in the parent environment
		u <<- NULL  # if it exists, we renew 
	}
	
	for_g <- function() x # the get function
	
	for_set_inv <- function(inverse) u <<- inverse
	for_get_inv <- function() u # prepare for getting the value 
	
	list(set=for_s,get = for_g,setinverse = for_set_inv, 
		 getinverse = for_get_inv)
}

## The function is for to calculate the inverse matrix
cacheSolve <- function(x, ...) {
	u <- x$getinverse()
	
	# we get the cache by the above function
	
	if (!is.null(u)) {
		# Check whether the inverse matrix is already for computing
		message("... we are loading the cached data...")
		return (u)
	}
	
	matrix_to_invert <- x$get()
	# Getting the cached matrix for determine the inverse
	u <- solve(matrix_to_invert, ...)
	# Calculating the inverse
	x%setinverse(u)
	# Return the inverse
	u
}
