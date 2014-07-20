## This function is aimed at the optimization of the inverse matrix counting,
## as matrix inversion is often a time-wasting computation.

## This function creates a special "matrix" object that can cache its inverse 
## to return it in case of matrix repetition.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i<<- solve
        getinverse <- function() i
        list(set=set, get=get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The next function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
