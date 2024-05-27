## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Caches the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        #Similar to the makeVector function in the assignment instructions
        inv <- NULL
        #Set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #get the value of the matrix
        get <- function() x
        ##set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
        #get the inverse of the matrix
        getinverse <- function() inv
        #return a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Similar to the cachemean function in the assignment instructions
        #Get the inverse of the matrix
        inv <- x$getinverse()
        #If the inverse is not null, return the cached data
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #If the inverse is null, calculate the inverse and cache it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
