## Comments
## This R file contains two functiosn that are able to inverse a matrix.
## The functionality of the functions work based on the below logic. If the 
## inverse of the matrix has already been calculated, then the same matrix is
## used to return the value. If not, an inverse is calulated fresh and then 
## returned.


## Calculate the inverse of a matrix
	makeCacheMatrix <- function(x = matrix()) {
  				cacheInverse <- NULL
    				set <- function(y) {
        			x <<- y
	    			cacheInverse <<- NULL
	      			}
	    # Get the matrix to be inverted
		get <- function() x
	    # Set the inverse of matrix to a matrix variable in cache
		setInverse <- function(inverse) cacheInverse <<- inverse
	    # Get the inverse of matrix from cache
		getInverse <- function() cacheInverse
	    # Set the variable values
		list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
		}

## Return the inverse of an cacheMatrix object
	cacheSolve <- function(x, ...) {
		# Return a matrix that is the inverse of 'x'
			inv <- x$getInverse()
		# If inverse exsits, then use the same
			if(!is.null(inv)) {
				message("Inverse exists in cache")
				return(inv)
			  	      }
		# If inverse not existing, then calculate inverse fresh
			matToBeInverted <- x$get()
			inv <- solve(matToBeInverted, ...)
	 	        x$setInverse(inv)
			inv
	   	}

