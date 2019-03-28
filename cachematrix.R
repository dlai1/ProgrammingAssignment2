## Put comments here that give an overall description of what your
## functions do

## Generates invertible matrix, has a cached inverse that can be stored later

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


## Solves and inverts the matrix, inverted matrix is stored and returned as m

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
