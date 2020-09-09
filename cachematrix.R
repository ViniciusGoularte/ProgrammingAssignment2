## Put comments here that give an overall description of what your
## functions do

## This function will provide a list of get and set functions for the
###given matrix and its respective inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #setting the null inverse
    set <- function(y) { #function to set matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x #function to get the matrix
    setinverse <- function(inverse) i <<- inverse #function determine the inverse
    getinverse <- function() i #function to get value from the matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse) # setting the return with the environment
}


## This function retuns the inverse of the matrix based in 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
