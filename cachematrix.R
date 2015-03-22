## These functions calculate and cache the inverse of a matrix.
## Using cached versions of objects instead of calculating them each time
## is more efficient and helps R run faster.

## "makeCacheMatrix" creates a cache-supporting matrix
makeCacheMatrix <- function(x = matrix()) {
        ##creates an object 'i' with an undefined value
        i <- NULL
        
        ## sets the value of the matrix 'x' to what is entered using "x$set(y)" 
        ## i is still an empty object
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## "get" retrieves the value of 'x'
        get <- function(){
                x    
        } 

        ## "setinverse" sets i equal to the value of the inverse
        setinverse <- function(inverse) {
                i <<- inverse
        }
        
        ## "getinverse" retrieves the local value of i
        getinverse <- function(){
                i
        }
        
        ## asks "makeCachematrix" to construct and return a list of
        ## the functions it uses, their values, and their environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function:
## 1) Retrieves a cached version of the matrix if it already exists
## 2) Calculates the inverse of the matrix if it is not already cached

cacheSolve <- function(x, ...) {
        ## sets the value of i equal to the inverse of the matrix
        ## using "getinverse" from "makeCachematrix"
        i <- x$getinverse()
        
        ## checks if a cached version of the inverse matrix already exists
        ## informs user and returns the inverse matrix
        ## stops the function from continuing if a cached version exists
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
## if the value of i is still "NULL", the function will continue

        ##retrieves the value of the matrix
        data <- x$get()

        ##calculates the value of the inverse matrix
        i <- solve(data, ...)

        ## sets the value of i for this matrix using "setinverse" 
        ##so that it can be retrieved from this cache
        x$setinverse(i)

        ##calls the inverse of the matrix
        i
}
