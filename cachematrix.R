## Here we are trying to resolve an issue with computing matrix inversion which
##can be costly and time-consuming (bc loops). Cache the inverse will save time
## Assumed matrix working with is always invertible

## makeCacheMatrix creates matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y) {           # set() enables valid matrix entry
                x <<- y                 #superassignment
                i <<- NULL              #supperassignment placeholder for future value
}
get <- function() x                     #returns vector x
setinverse <- function(inverse) i <<- inverse           
getinverse <- function() i                              #returns inverse
list(set = set,                                         #returns vector containing all defined functions
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}                                                       #always check to close function...

## Below function computes inverse of makeCacheMatrix matrix
## cacheSolve should retrieve inverse if makeCacheMatrix has retrieved from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
invd <- x$getInverse()
        if (!is.null(invd)) {           #check whether original matrix has been modified
                message("get cached matrix")
                return(invd)
                }
        matx <- x$get()
        inversed <- solve(matx, ...)
        x$setInverse(invd)
        invd
}
