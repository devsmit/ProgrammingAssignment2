#initializing function makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
#setting inverse to null
    invs <- NULL
#fnc to set value of matrix
    set <- function(y) {
        x <<- y
        invs <<- NULL
    }
#fnc to get value of matrix
    get <- function() x
#fnc to set inverse value of matrix
    setinvs <- function(inverse) invs <<- inverse
#fnc to get inverse value of matrix
    getinvs <- function() inv
#list object to return all functions
    list(set=set,
	 get=get,
	 setinvs=setinvs, 
	 getinvs=getinvs)
}


cacheSolve <- function(x, ...) {
#assigning invs by get inverse
    invs <- x$getinvs()
    if(!is.null(invs)) {
        message("getting cached data.")
        return(invs)
    }
    data <- x$get()
    invs <- solve(data)
    x$setinvs(invs)
    invs
}
