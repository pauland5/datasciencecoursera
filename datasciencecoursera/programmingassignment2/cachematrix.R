##This script finds the inverse matrix and caches it

#This function initializes the memory
makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inversematrix <<- solve
    getinverse <- function() inversematrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##This function checks to see if the inverse is cached, if so it passes that value back
##If the inverse is not cached a new inverse matrix is calculated using the solve command
cacheSolve <- function(x = matrix()) {
    inversematrix <- x$getinverse()
    if (!is.null(inversematrix)) {
        message("getting cached matrix")
        return(inversematrix)
    }
    matrix <- x$get()
    inversematrix <- solve(matrix)
    x$setinverse(inversematrix)
    inversematrix
}