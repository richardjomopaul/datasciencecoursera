

## this function creates a "special" matrix object from an existing 
## "regular" matrix object, that in addition to storing
## the matrix itself, it can store the value of its mean

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## this function returns the mean of the special matrix, 
## it checks if the mean value in the object is null
## if not null it returns value stored, if null it calculates,
## stores and then returns value



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

}
