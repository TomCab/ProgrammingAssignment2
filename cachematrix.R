## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a list of 4 functions
## 1. set : Sets the value of matrix
## 2. get : Gets the value of matrix
## 3. setInverse : Sets the value of the Inverse
## 4. getInverse : Gets the value of the Inverse
## ============================================================================
## Execution
# > source('/Volumes/Data/Coursera/R/cachematrix.R')
# > mat <- matrix(c(1:4),2,2)
# > mat_cache <-makeCacheMatrix(mat)
# > mat_cache
# $set
# function (y) 
# {
#        x <<- y
#         m <<- NULL
# }
# <environment: 0x1082d7390>
#         
# $get
# function () 
# x
# <environment: 0x1082d7390>
#         
# $setInverse
# function (inv) 
# m <<- inv
# <environment: 0x1082d7390>
#         
# $getInverse
# function () 
# m
# <environment: 0x1082d7390>


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## The cacheSolve function checks if the matrix inverse exists by the call 
## x$getInverse(). In case the inverse exists it returns the cached matrix
## else it computes the inverse using the solve function and returns the 
## Inverse
## Execution
# > cacheSolve(mat_cache)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mat_cache)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}