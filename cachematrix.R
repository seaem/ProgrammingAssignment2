#Function makeCacheMatrix is used to get and set the matrix as well as get and set the inverse
#Function cacheSolve calculates the inverse. But skips the computation and returns the inverse from cache if cache has already been calculated

#This function returns a list containing a function to :
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of matrix
#4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


# This function calculates the inverse of matrix. 
# But if inverse has already been calculated, it returns the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
