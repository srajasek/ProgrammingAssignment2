## makeCacheMatrix has 4 functions: set, get, setinverse, getinverse
## get: Gets the matrix x stored in the main function
## set: Changes the matrix stored in the main function
## setinverse: Sets the inverse into the variable i, doesn't do anything else
## getinverse: Gets the value of i from main function, doesn't do anything else
## list: stores the 4 functions of the main function, so that when we assign makeCacheMatrix to an onject, the object has the 4 functions.

## This is the main function. It gets, sets the matrix and also caches the matrix supplied and the matric inverse as well.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gets the matrix inverse, if it exists, it uses the cached value, otherwise it computes the inverse and assigns it to i.

cacheSolve <- function(x, ...) {
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
