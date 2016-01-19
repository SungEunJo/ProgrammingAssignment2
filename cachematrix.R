## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #initialize inverse matrix of x
        set <- function(y) { # function that sets x to y // set matrix value
                x <<- y
                m <<- NULL
        }
        get <- function() x # function that returns x //get matrix value
        setInverse <- function(solve) m <<- solve # function that sets m to solve // set inverse matrix value
        getInverse <- function() m # function that returns m // get inverse matrix value
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
#x: a matrix object created by makeCacheMatrix() function
#...: other arguments passed to solve() function

        m <- x$getInverse()
        if(! is.null(m)) { # if already stored
                message("getting cached data")
                return(m) # return cached inverse matrix of x
        }
        # calculate and cache the inverse matrix of x
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m) # cache
        m # return inverse matrix of x
}
