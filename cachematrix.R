## Calculating the inverse of a matrix can be a time consuming task for computers.  This script creates a special 
#matrix and caches the inverse of it to prevent repeated time consuming calculations of the inverse.

##The makeCacheMatrix creates the special "matrix" object storing the matrix and the cachesolve calculates the 
#inverse of a matrix made from makeCacheMatrix.


##This function takes in any matrix and returns a special matrix object containing the matrix, the Inverse of the 
#matrix if it has been calculated and information on if the inverse has been calculated.

makeCacheMatrix <- function(x =  matrix()){
        m <-  NULL
        setMatrix <-  function(y){
                x <<- y
                m <<- NULL
        }
        getMatrix <-  function() x
        cacheInverse <- function(solve) m <<- solve
        getInverse <-  function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## This function calculates the Inverse of a matrix object created with makeCachematrix.  
#To save time it first checks if the inverse has already been calculated and if so it returns the cached data.  
#If not it calcualtes the inverse and caches that in the matrix object
cachesolve <- function(x){
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data)
        x$cacheInverse(m)
        m
}
