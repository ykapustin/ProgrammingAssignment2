## That set of functions caches result of costly operation of computation of 
## matrix inversion. It is useful when we need result of the computation
## is being used multiple times

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL

    ## define function set
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## define function get
    ## get the value of the matrix
    get <- function() x

    ## define function setInversion
    ## set the value of the matrix inversion calculation
    setInversion <- function(inversion) i <<- inversion

    ## define function getInversion
    ## get the value of the matrix inversion calculation
    getInversion <- function() i

    ## return the object with functions
    list(set = set, get = get,
         setInversion = setInversion,
         getInversion = getInversion)
    
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## attempt to get matrix inversion from the cache
    m <- x$getInversion()
    
    ## if matrix inversion defined, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    ## get matrix, that needs to calculate inversion
    data <- x$get()

    ## calculate matrix reversion
    m <- solve(data, ...)
    
    ## save result in cache
    x$setInversion(m)

    ## Return a matrix that is the inverse of 'x'
    m    
}
