##R programming Assignment
##The function  below initialises x to an empty matix and the inverse to a NULL value
##A function named set is declared in which the value of the matrix and the inverse is cached
##The value of the matrix is passed into the function get
##A function named setinverse is declared which calculates the inverse of the matrix and the inverse is passed into a function getmatrix
##A list with this functions is then returned

makeCacheMatrix <- function(x = matrix()) {
		inverse <- NULL
        set <- function(yy) {
                x <<- yy
                inverse <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) inverse <<- solve
        getmatrix <- function() inverse
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The function below gets the cache of the matrix. The function checks if the value of the inverse exists, if the inverse exists
## it returns the cached data else the inverse is calculated and then obtained

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getmatrix() 
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setmatrix(inverse)
        inverse
}
