## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Makes a list containing functions to set and get a matrix, as well
## as set and get its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse = matrix()) inv <<- inverse
        getinverse <- function() inv
        list( set = set, get = get,
              setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## Checks to see if the inverse of the matrix has already been calculated, then
## calculates it and returns is as needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <-solve(data,...)
        x$setinverse(inv)
        inv
}