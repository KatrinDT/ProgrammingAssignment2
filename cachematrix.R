## The functions below allow to get the inverse for a square matrix with the
## solve command. As computation might be tedious and time-consuming, the cacheSolve 
## function checks first whether the inverse already exists and does not
## need to be calculated again.

## makeCachematrix enables storage of the inverse of a matrix in cache and provides
## a list with elements that are used in cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve either caches the inverse of the matrix if already calculated
## or it calculates the inverse using solve

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    
    # check whether inverse is already saved in cache
    # and return it if this is the case
    
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached inverse data")
        return(m)
    }
    
    
    # get the data
    data <- x$get()
    # solve A = inverse of A where A is a square matrix
    m <- solve(data,...)
    x$setinv(m)
    m
    
    }


