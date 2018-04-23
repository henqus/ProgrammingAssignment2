## The functions in this code cache a square matrix and its inverse
##  >  y <- makeCacheMatrix(x)   stores the matrix
##  >  cacheSolve(y)             retrieve the inverse matrix

## makeCacheMatrix: This function creates a function list to cache a
## matrix and its inverse.
## cache the matrix with   > y <- makeCacheMatrix(x)
## retrive the matrix with > y$get()
## cache the inverse of x     > y$setinverse(solve(y$get)))
## retrieve the inverse of x  > Y$getinverse()

makeCacheMatrix <- function(x = matrix()) { 
        invm <- NULL
        set <- function(y) {
                x <<- y
                invm <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) invm <<- solve
        getinverse <- function() invm
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve retrieves the inverted matrix repeatedly but calculates it only once
## cahesolve(y)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversem <- x$getinverse()
        if(!is.null(inversem)) {
                message("getting cached data")
                return(inversem)
        }
        data <- x$get()
        inversem <- solve(data, ...)
        x$setinverse(inversem)
        inversem
}
