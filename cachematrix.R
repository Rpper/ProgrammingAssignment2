#This function creates a special inversed matrix object which store in cache 
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse matrix
#get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
       #invmatrix store inversed matrix
        invmatrix <- NULL
        #set input matrix
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        # get original matrix
        get <- function() x
        # create inversed matrix using solve function
        setinvmatrix <- function(inverse) invmatrix <<- inverse
        #get inversed matrix
        getmatrix <- function() invmatrix
        #return function list.
        list(set = set, get = get, setinvmatrix = setinvmatrix,
        getmatrix = getmatrix)        
}

#calculates the inversed matrix with above function. if the inversed matrix has already been calculated.
# If so, it gets the matrix from the cache and skips the computation. #Otherwise, it calculates the inverse 
# matrix using above function.o
cacheSolve <- function(x, ...) {
        invmatrix <- x$getmatrix()
        #search to see if invmatrix have been calculated and stored in cache.
        if(!is.null(invmatrix)) {
                message("getting cached data")
                return(invmatrix)
        }
        #get data and calculated inversed data using above function.
        data <- x$get()
        invmatrix <- solve(data, ...)
        x$setinvmatrix(invmatrix)
        #return invmatrix
        return(invmatrix)
}