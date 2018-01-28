## The makeCacheMatrix function creates a special matrix, which is a list containing a function to set and get the value to the matrix, set and get the value of the inverse of the matrix.
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(Solve) inv <<- Solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
          
}


## The cacheSolve function calculates the inverse of the special vector created with the function above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        data <- x$get()
        if(det(data)!= 0){
        	inv <- solve(data, ...)
        } else { message("inverse doesn't exist for the matrix") }
        
        x$setinv(inv)
        inv
}
