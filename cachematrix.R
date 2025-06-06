## Put comments here that give an overall description of what your
## functions do

## This function makeCacheMartix, creates a special "matrix" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inve <<- inverse
        getInverse <- function() inve
        list(set=set,get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}


## This function calculates the inverse of the special "matrix" created with the above funtion.
## However it first checks to see if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inve <- x$getInverse()
        if(!is.null(inve)) {
                message("getting cached data")
                return(inve)
        }
        mat <- x$get()
        inve <- solve(mat,...)
        x$setInverse(inve)
        inve
}

test1 <- matrix(rnorm(15,2,0.5),4,4)
test2 <- makeCacheMatrix(test1)
cacheSolve(test2)
