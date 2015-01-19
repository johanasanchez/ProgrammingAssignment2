## The makeCacheMatrix function, creates a special "matrix" object that can cache its inverse. Which is really containing a function to 
##    set the value of the matrix
##    get the value of the matrix
##    set the value of the matrix inverse
##    get the value of the matrix inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) matinv <<- inverse
        getinv <- function() matinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        matinv <- x$getinv()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data)
        x$setinv(matinv)
        ## Return a matrix that is the inverse of 'x'
        matinv
}
