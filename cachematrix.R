## The function makeCacheMatrix is used to calculate the inverse of the matrix
## The solve function is used to inverse a matrix in R

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y)
{
x <<- y
inv <<- NULL
}
get <- function() x
setInv <- function(inverse) inv <<- inverse
getInv <- function() inv
list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## this function computes the inverse of the special matrix returned by makeCacheMatrix above. If the inverse has already been calculated
## then the cacheSolve would retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
if(!is.null(inv))
{
message("getting cached data")
return(inv)
}
mat <- x$get()
inverse <- solve(mat,...)
x$setInv(inv)
inv    
}
