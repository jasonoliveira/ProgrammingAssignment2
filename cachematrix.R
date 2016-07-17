## Homework Assignment 2, week 3 
## The first function creates a list containing 4 fucntions to establish the Matrix
## The second function that Inverts the matrix that you establish. If it was done already, returns the cached version
## otherwise it create the inverted square matrix, assuming the matrix is invertable. 

## Establish and seed the matrix and the calleable set of functions to set, get, setInverse, GetInverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function inverts a matrix created by makeCacheMatrix. If the inversion is done, returns the cached version
## If the inversion is NULL, calculates the inversion through Solve()

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
