

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    #create the object m to be used later    
    i <- NULL
        #create a set function to set the value x and to make sure i is set to NULL
        #when there is a new x submitted to the function
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        
        #creates a get function to allow cacheSolve to access the value of x
        get <- function() x
        #sets a value for i of the inverse of x to be stored in cache
        setinverse <- function(inverse) i <<- inverse
        #allows cacheSolve to access the cached inverse of x
        getinverse <- function() i
        #orders these functions in a named list so cacheSolve can call specific functions based on name
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    }



## returns the inverse of a matrix x, using the cached data from makeCacheMatrix

cacheSolve <- function(x, ...) {
    #sets the value of i locally to be the value of i in MakeCacheMatrix
    i <- x$getinverse()
    #if i isn't empty it returns the value of i previously stored in MakeCacheMatrix
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    #calls the original input in MakeCacheMatrix
    data <- x$get()
    #solves for the inverse of that Matrix
    i <- solve(data, ...)
    #sets the inverse of x to i globally using the function from MakeCacheMatrix
    x$setinverse(i)
    #returns the inverse of the original matrix
    i
}

