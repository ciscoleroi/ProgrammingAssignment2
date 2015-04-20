## The below functions enable the caching of inverted matrices. 
## This way, inverted matrices have to be computed only once 
## for latter use. 

## This function creates the matrix-instance, which stores 
## the matrix, and if computed, its inverse.

makeCacheMatrix <- function(x = matrix()) 
	{
        inv <- NULL
        
        createCachableMatrix <- function(y) 
        	{
                x <<- y
                inv <<- NULL
        	}
        	
        getCachedMatrix <- function() x
        
        setInvertedMatrix <- function(inverse) inv <<- inverse
        
        getInvertedMatrix <- function() inv
        
        list(createCachableMatrix = createCachableMatrix, 
             getCachedMatrix = getCachedMatrix,
             setInvertedMatrix = setInvertedMatrix,
             getInvertedMatrix = getInvertedMatrix)
	}

## This function checks if the inverse of a specific 
## matrix-instance has already been computed. If yes,
## it returns the cached value, if no, it computes the 
## inverse stores it in the instance and returns the inverse.

cacheSolve <- function(x, ...) 
	{
        inv <- x$getInvertedMatrix()
        
	if(!is.null(inv)) 
		{
		message("getting cached inverse")
	        return(inv)
	        }
	        
	 mat <- x$getCachedMatrix()
	 
	 inv <- solve(mat, ...)
	 
	 x$setInvertedMatrix(inv)
        
       	inv
	}
