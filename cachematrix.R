## Two functions, makeCacheMatrix and cacheSolve, are desinged to take advantage of 
## caching data to speed the computatonal process involving matix inversion.

## makeCacheMatrix is a funciton that creats a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(b) {
        x <<- b
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## cachesolve is a function that camputes the inverse of a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
