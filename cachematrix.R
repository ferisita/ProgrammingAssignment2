## The first function creates a "matrix" object able to cache its inverse, 
#while the second function computes the inverse of the special "matrix" 

## This function creates a "matrix" object able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinversa <- function() inversa <<- solve(x) 
        getinversa <- function() inversa
        list(set = set,
             get = get,
             setinversa = setinversa,
             getinversa = getinversa)
}


## returns the inverse of x

cachesolve <- function(x, ...) {
        matx <-matrix()
        m <- x$getinversa()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matx <- x$get()
        m <- solve(matx, ...)
        x$setinversa <- m
        m
}
