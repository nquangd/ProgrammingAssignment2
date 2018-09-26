## This code is to calculate the inverse of a square matrix (if such an inverse exists), 
## or cache the inverse from the memory if it has been calculated before


## This function is to create a "matrix" which is actually a 4-elemments list that is composed of 4 functions to: 
## (i) set the value the input matrix (to avoid re-initialising), (ii) get the value of the input matrix, (iii) set the value of the inversed matrix, 
## and (iv) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		cachedinverse <- NULL
		## if one wants to change the value of the input matrix without initialising another object
		set <- function(y) {
				x <<- y
				cachedinverse <<- NULL
		}
		
		get <- function() x
		setinverse <- function(inverse) cachedinverse <<- inverse
		getinverse <- function() cachedinverse
		list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
		
}


## This function utilises the output returned by the above function to calculate the inverse of a matrix. 
## (i) if the inverse has been calculated, it gets the value of the inverse matrix from the cache
## (ii) if the inverse has not been calculated, it will first check if the input matrix has an inverse (condition: det(matrix) !=0).
## if that is the case, the inverse will be calculated (and stored in the cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        cachedinverse <- x$getinverse()
        if(!is.null(cachedinverse)) {
        		message("gettting cached data")
        		return(cachedinverse)
        }
        
        data <- x$get()
        if (det(data)==0) {
        		message("The input matrix has no inverse")
        }
        else {
        cachedinverse <- solve(data,...)
        x$setinverse(cachedinverse)
        cachedinverse
        }
            
}
