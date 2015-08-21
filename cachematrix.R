## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## "makeCacheMatrix" will add plumping to an object encapsulating a matrix which will allow the caching of the matrix's inverse.
## the initial call from cacheSolve will compute and cache the inverse
## subsequent calls to x$getInverse() will check/retrieve the cached inverse

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL             ## m holds the cached inverse matrix after it has been commputed
    
    ## define the sub functions
    set <- function(y){
        x <<- y           ## set the value of the incoming matrix to x in parent environment
        m <<- NULL         ## set m  in the parent environment
    }
    get <- function()x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    
    ## map the function entries to this object's names
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

##  "cacheSolve" calls an object of makeCacheMatrix.
##  the first call will create and cache the inverse matrix into the object
##  subsequent calls to "cacheSolve" on the object will return the cached inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## attempt to get matrix inverse from cache
    m <- x$getInverse()
    if(!is.null(m)){
        message("getting cached matrix")
        return(m)         ## return found inverse
    }
    
    ## matrix inverse not found so call solve to compute it
    data <- x$get()       ## get the incoming matrix
    m <- solve(data,...)  ## compute the inverse
    x$setInverse(m)       ## put matrix inverse into the cache
    m
}
