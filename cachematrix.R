## These two functions, makeCacheMatrix and cacheSolve, calculate the inverse of a matrix and
## store the resulte within a cache. 

## This function takes a square matrix as an argument and creates a list containing three functions
## described inline.

makeCacheMatrix <- function(mat=matrix()) {
     
     inv <- NULL # here the inverse matrix is initialized as NULL
     get <- function() mat # get returns the initial matrix 
     setinv <- function(solution) inv <<- solution # the solution is stored into inv, using a calculated of cached value
     getinv <- function() inv # getinv returns the inverse matrix
     list(get=get, setinv=setinv, getinv=getinv)
     
}

}


## This is the function in which the inverse matrix is either calulated or retrieved from the cache

cacheSolve <- function(mat, ...) {
     
     inv <- mat$getinv() # getinv function is called to read the value stored in inv 
     # is inv is not NULL, the cached value is used and a flag message is printed together with the result
     if (!is.null(inv)) {   
          message("getting cached value")
          return(inv)
     }
     
     # is inv is NULL, the inverse matrix is used using the solve function and the result is stored in the cache 
     matrix <- mat$get()
     inv <- solve(matrix)
     mat$setinv(inv)
     inv
     
}
