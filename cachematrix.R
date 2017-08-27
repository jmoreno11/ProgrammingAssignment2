
## Caching the Inverse of a Matrix:
## This function creates a special "matriz" object than can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,  get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. It should retrieve the inverse from the cache.



cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'mat'
        inv <- mat$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matx <- mat$get()
        inv <- solve(matx, ...)
        mat$setInverse(inv)
        inv
}
