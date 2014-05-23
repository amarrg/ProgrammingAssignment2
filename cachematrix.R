## makeCacheMatrix creates a special matrix that caches its inverse
## This creates a special matrix that is really a list to
## set the value of matrix, get value of matrix, set value of inverse, get value of inverse

makeCacheMatrix <- function(x = matrix()){
    inv <- NULL
    set <- function(y){
    x <<- y
    inv <<- NULL
    }

    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
	 getinv = getinv)
}

## cacheSolve function calculates the inverse of the special matrix
## It first checks if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the computation
## Else, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via the setinv function

cacheSolve <- function(x, ...){
     inv <- x$getinv()
     if(!is.null(inv)) {
	message("getting cached data")
	return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}
