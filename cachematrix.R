## 2 functions to cache the inverse of a matrix

## Function to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set<- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInv<- function(inverse) invMatrix <<- inverse
        getInv<- function() invMatrix
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInv()
        if (!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        data<- x$get()
        invMatrix<- solve(data, ...)
        x$setInv(invMatrix)
        invMatrix
}
