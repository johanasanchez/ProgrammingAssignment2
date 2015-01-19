## The makeCacheMatrix function, creates a special "matrix" object that can cache its inverse. Which is really containing a function to 
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the matrix inverse
##    get the value of the matrix inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y){
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) matinv <<- solve
        getinverse <- function() matinv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       matinv <- x$getinverse()
        if (!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data)
        x$setinverse(matinv)
        ## Return a matrix that is the inverse of 'x'
        matinv
}
