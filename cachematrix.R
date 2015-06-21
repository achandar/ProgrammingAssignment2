makeCacheMatrix <- function(x = matrix()) {
        # x is a square invertible matrix
        # return: a list containing functions to
        #             1. set the matrix
        #              2. get the matrix
        #              3. set the inverse of the matrix
        #              4. get the inverse of the matrix
        # These lists are used as input for cacheSolve()
        inv = NULL
        set = function(y) {
                x <<- y #from Example : used to assign a value to an object in an environment 
                #that is different from the current environment
                inv <<- NULL
        }
        get = function() x
        setinverse = function(inverse) inv <<- inverse 
        getinverse = function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
        # x: output of makeCacheMatrix()
        # return: inverse of the original matrix input to makeCacheMatrix()
        
        inv = x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(inv)){
                # get it from the cache and skips computational overhead 
                message("getting cached data")
                return(inv)
        }
        
        # If it's not in cache, get the inverse 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache
        x$setinverse(inv)
        
        return(inv)
}
