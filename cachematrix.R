## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        matrixInv <- NULL
        set <- function(y) {
                x <<- y
                matrixInv <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) matrixInv <<- Inv
        getInv <- function() matrixInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInv <- x$getInv()
        if(!is.null(matrixInv)) {
                message("getting cached data")
                return(matrixInv)
        }
        data <- x$get()
        matrixInv <- solve(data, ...)
        x$setInv(matrixInv)
        matrixInv
}
