## David Ferguson || Coursera rprog-030 || Programming Assignment 2 || 7/17/2015

## Function to allow creation of a matrix with the capability to cache its inverse
## and an associated function to check if the inverse is cached before calculating it

## Create new cache-able matrix container with functions for setting/getting the
## matrix and functions for setting/getting the inverse
makeCacheMatrix <- function( new_matrix = matrix() ) {

	matrix_inverse <- NULL

	setMatrix <- function( new_matrix ){
		this_matrix <<- new_matrix
		inverseMatrix <<- NULL
	}
	getMatrix <- function(){
		return( this_matrix )
	}

	setInverse <- function( new_inverse ){
		matrix_inverse <<- new_inverse
	}
	getInverse <- function(  ){
		return( matrix_inverse )
	}

	list( 	setMatrix=setMatrix, 	getMatrix=getMatrix,
			setInverse=setInverse, 	getInverse=getInverse )

}


## Check if matrix's inverse is already cached. If so, return the inverse. If not,
## calculate the inverse, cache it, and return it
cacheSolve <- function(x, ...) {

	timer <- proc.time()

	inverse <- x$getInverse()

	if( !is.null( inverse ) ){
		return( inverse )
	}

	var_matrix <- x$getMatrix()
	inverse <- solve( var_matrix )
	x$setInverse( inverse )

	return( inverse )

}