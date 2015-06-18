## Coursera R Programming ProgrammingAssignment2
## The function is used for caching the inverse of a Matrix

## Below has two functions to cache the inverse of a matrix to save 
## the excessive computation. 

## The first funtion creats a special "matrix" object that can cache 
## it's invese. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set=set, 
             get = get, 
             setInverse=setInverse, 
             getInverse=getInverse)
}


## below function This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("Now getting the result from cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
