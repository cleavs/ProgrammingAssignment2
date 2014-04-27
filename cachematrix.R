## This function creates a special "matrix" object that can cache its inverse

## The makeCacheMatrix function is a list of 4 functions that:
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse matrix
##4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        cMatrix <- NULL
        set <- function(y){
                x <<- y
                cMatrix <<- NULL
        }
        get <- function() {
                x
        }
        setinv_matrix <- function(solve) cMatrix <<- solve
        getinv_matrix <- function() cMatrix
        list(set = set, get = get,
             setinv_matrix = setinv_matrix,
             getinv_matrix = getinv_matrix)
        
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv_matrix(m)
        m
}
