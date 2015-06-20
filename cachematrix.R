## makeCacheMatrix and cacheSolve returns the inverse of a matrix if
## it exists, and if the inverse was calculated then cacheSolve
## returns the cached inverse matrix

## makeCacheMatrix takes in a matrix and make a list of functions to
## be used in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	## set is a function to set the new matrix
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## get is a function to produce the matrix
	get <- function() x

	## setinverse saves the inverse of the matrix
	setinverse <- function(inv) i <<- inv
	
	## getinverse produces the saved inverse
	getinverse <- function() i

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## cacheSolve first see if the inverse has been calculated. If the inverse
##   has been calcualted, then it returns the saved inverse. Else, it
##   calculates, saves, and produces the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	matx <- x$get()
	m <- solve(matx, ...)
	x$setinverse(m)
	m
}
