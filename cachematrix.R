## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# a list of functions will be generated: 
# getCache returns the stored cache, 
# getMatrix returns the matrix for which the inverse was calculated
# setMatrix stores a new matrix 
# setInverse stores the new inverse calculation
# hasChanged tells if the Matrix has changed since the last updating of the inverse

        i <- NULL       
        changed <- FALSE
        getCache <- function() i
        getMatrix <- function() x
        setMatrix <- function(y) {
                x <<- y
                changed <<- TRUE
        }
        setInverse <- function(y) {
                i <<- y
                changed <<- FALSE
        }
        hasChanged <- function() changed
        list(getCache = getCache, getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, hasChanged = hasChanged)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # if the cached inverse is not NULL 
        # and the matrix has not changed since last inverse value storing 
        # then we return cached value 
        # otherwise we calculate the inverse of the matrix currently stored in X and we store the result
        # as well as returning this new value
        
        if ((x$hasChanged() == FALSE) && (!is.null(x$getCache()))) {
                message("getting cached data")
                return(x$getCache())
                
        }
        data <- x$getMatrix()
        i <- solve(data)
        x$setInverse(i)
        i
}
