## Assignment 2 Coursera Solutions

## Generates invertible matrix, has an object m where the inverse can be stored later

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
	  get <- function() x
        	
	  setinv <- function(inverse) m <<- inverse
          getinv <- function() m
          list(set = set, 
	       get = get,
	       setinv = setinv,
	       getinv = getinv)
}

## Solves and inverts the matrix, inverted matrix is stored and returned as object m

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        ## If a matrix is already cached, it is returned without solving again
	if(!is.null(m)) {
                message("getting cached data")
                return(m) 
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
