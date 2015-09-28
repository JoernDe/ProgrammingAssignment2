## these functions are supposed to cache the inverse of matrix "x" and to retrun the inverse 
## if it has been already calculated, if not it will be calculated

## the first function "makeCacheMatrix" creates a matrix "x"
## gets the matrix, inverts the matrix and gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<-solve
    getmatrix <- function() m
    list (set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
    
}


## this function calculates the inverse of the matrix above, if this has not been done already, 
## otherwise it states "getting inverse matrix"

cacheSolve <- function(x, ...) {
    m<- x$getmatrix()
    if(!is.null(m)) {
        message("getting inverse matrix")
        return(m)
    }
    data<- x$get()
    m <- solve(x, ...)
    x$setmatrix(m)
    m
    ## Returns a matrix that is the inverse of 'x'
}
