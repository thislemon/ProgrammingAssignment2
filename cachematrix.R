##Pair of functions to cache the inverse of a matrix

##Provides a list of functions to get, set and cache the inverse of a matrix
makeCacheMatrix <- function(z = matrix()) {
        inv <- NULL
        get <- function() z
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}

##This function tries to get the inverse from cache, if it doesn't exist, it inverses the data and sets it
cacheSolve <- function(z, ...) {
        inverse <- z$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- z$get()
        inverse <- solve(data)
        z$setinverse(inverse)
        inverse
}