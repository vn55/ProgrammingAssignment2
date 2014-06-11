## This set of two functions efficiently returns the inverse of a matrix
## by creating an object to store the matrix and its inverse.
## When the inverse of a matrix is computed, it is stored in the 
## cache so that future calls to the inverse of the same matrix do not need
## to be recomputed. This process saves time and computational resources.



## Function 1. This function defines the object and the commands to store and 
## retrieve a matrix, and 
## commit it to the cache, together with its inverse.

makeCacheMatrix <- function(x = matrix()) {

	   ## first, our output variable 'inv' is set to NULL

	   inv <- NULL

	   ## then the 'set' command is defined. This command is used to
	   ## 'store' a matrix. For example: 
	   ## a <- makeCacheMatrix()
	   ## a$set(matrix(c(4,2,7,6),2,2)) 

	   set <- function(y) {
	   		x <<- y
			inv <<- NULL
	   }

	   ## then the get command is defined. This command is used to 
	   ## 'retrieve' a matrix. For example:
	   ## a$get() will return the example matrix created above
	
	   get <- function() x

	   ## then the commands to store and retrieve the inverse matrix are defined. 

	   setinv <- function(solve) inv <<- solve
	   getinv <- function() inv

	   ## this function outputs the 4 commands to the screen:
	   list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Function 2. This function returns the matrix that is the inverse of 'x'. 
## 'x' is based on the data defined with the previous function.

cacheSolve <- function(x, ...) {
        ## first, the function retrieves the inverse matrix previously stored, 
	  ## if this calculation has been done before. In case the calculation
        ## has not been done before, it will return NULL.

	  inv <- x$getinv()

	  ## If it does not get back NULL, it will use the previously stored
	  ## inverse matrix and it will output a message to inform the user.

	  if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
	  }

	  ## If instead it gets back NULL, it will get the matrix, calculate its
	  ## inverse, and commit it to the cache. 

	  data <- x$get()
  	  inv <- solve(data,...)
	  x$setinv(inv)

	  ## the function returns the inverse matrix.

	  inv
}

