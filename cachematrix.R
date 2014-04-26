## Special wrapper of a matrix in cases where we need cheaper repeat access to the inverse.

## Build the matrix wrapper
makeCacheMatrix <- function(x = matrix()) {
	## by default we do not have an inverse.
	inv <- NULL

	## Allow the caller to set a new matrix in place of the use used at initialisation.
	set <- function( y ) {
		x <<- y
		## make sure an eventual cached inverse is cleared
		inv <<- NULL
	}
	
	## Return the matrix
	get <- function() {
		x
	}
	
	## cache the inverse
	setinverse <- function( inverse ) {
		inv <<- inverse
	}
	
	## return the inverse
	getinverse <- function() {
		inv
	}
}


## Given an object returned by makeCacheMatrix, calculate the inverse and cache it for further calls.
## If a cached inverse has already been calculated on the wrapped matrix, the cached version is returned.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	inv <- x$getinverse

	if( ! is.null( inv ) ) {
		## The wrapped matrix has a non NULL inverse
		message( "cache hit" )
		return inv
	}

	## Calculate the inverse using solve
	## TODO: handle special cases more carefully, not all matrices can be inverted.
	inv <- solve( x$get() )

	## Cache the result of solve()
	x$setinverse( inv )

	## return the inverse
	inv
}