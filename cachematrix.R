## The makeCacheMatrix function, creates a special "matrix" object that can cache its inverse. Which is really containing a function to 
##    set and get the value of the matrix
##    set and get the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y){
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matinv <<- solve
        getinv <- function() matinv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       matinv <- x$getinv()
        if (!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data)
        x$setinv(matinv)
        ## Return the inverse matrix  of 'x'
        matinv
}
