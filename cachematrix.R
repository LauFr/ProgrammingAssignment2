## The 'makeCacheMatrix' function creates a special object 'x': a squared matrix
## that can cache its inverse.
##
## The 'cacheSolve' function computes the inverse of the squared matrix,
## returned by the makeCacheMatrix function.
## If the inverse has already been calculated and the matrix has not changed,
## then the 'cachesolve' should retrieve the inverse from the cache.



## FUNCTION: makeCacheMatrix
##
## 1. The function evaluates if 'x' is a square matrix, in order to prevent
##    errors. If not, then the function returns a warning massage.
## 2. If 'x' is a squared matrix, then the function evaluates if 'x' is
##    invertible, in order to prevent errors.
##    If not, then the function returns a warning massage.
## 3. If 'x' is a squared invertible matrix, the function generates a list
##    of 4 objects:
##      a) the cache value of the matrix
##      b) the got value of the matrix
##      c) the cache value of the inverse
##      d) the got value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## 1.
    if(nrow(x)!=ncol(x)){
        message("WARNING: x is not a square matrix")
    }
    ## 2.
    else if(nrow(x)==ncol(x) && det(x)==0) {
        message("WARNING: x is not invertible")
    }
    ## 3.
    else {
        inv <- NULL
        ## 3a.
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        ## 3b.
        get <- function() x
        ## 3c.
        setinv <- function(solve) inv <<- solve
        ## 3d.
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }
}



## FUNCTION: cacheSolve
##
## 1. The function checks if the inverse has already been calculated. In this
##    case, the cached value is returned.
## 2. If not, the function calculates the inverse and caches its value.

cacheSolve <- function(x, ...) {
    ## 1.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("MESSAGE: getting cached data")
        return(inv)
    }
    ## 2.
    message("MESSAGE: calculating inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
