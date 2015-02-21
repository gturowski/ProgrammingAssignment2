# The first function, makeCacheMatrix creates a special "vector",
# which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      invers <- NULL
            set <- function(y) {
	                x <<- y
			            invers <<- NULL
				          }
					        get <- function() x
						      setinverse <- function(inverse) invers <<- inverse
						            getinverse <- function() invers
							          list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
								  }

# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been calculated. 
# If so, it gets the inversed matrix from the cache and skips the computation.
# Otherwise, it computes the inverse and sets the value in the cache via
# the setinverse function.


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      invers <- x$getinverse()
            if(!is.null(invers)) {
	                message("getting cached data.")
			            return(invers)
				          }
					        data <- x$get()
						      invers <- solve(data)
						            x$setinverse(invers)
							          invers
								  }
