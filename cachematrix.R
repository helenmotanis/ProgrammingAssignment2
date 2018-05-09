## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #assign inv to NULL value
        #setting the x matrix 
        set <- function(y) {
                  x <<- y
                 inv <<- NULL #assign inv to NULL value
        }
        #getting the matrix
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates that inverse if it's not already calculated (then it returns the cached results)

        cacheSolve <- function(x, ...) {
                inv <- x$getinv()
                
                ##check if there is already an inv - if yes return it
                
                if(!is.null(inv)) {
                       message("getting cached result")
                         return(inv)
                }
                
                #calculate the inverse when not exsisting
                data <- x$get()
                inv <- solve(data, ...)
                x$setinv(inv)
                inv
        }	 

