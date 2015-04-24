## This function sets the value of the matrix, gets the value of matrix,
## set the value of inverse of matrix and get the value of matrix 

makeCacheMatrix <- function(x = matrix()) {
m = NULL
set = function(y)
{
	x <<- y
	m <<- NULL
}
get <- function() x
setmatrixinverse <- function(solve) m <<- solve
getmatrixinverse <-function() m
list(set=set, get=get,
   setmatrixinverse=setmatrixinverse,
   getmatrixinverse=getmatrixinverse)

}


## This function calculates the inverse of a given INVERTIBLE matrix
## While calculating, it first checks the Cache if the inverse of the 
## matrix has already been calculated and cached. If so, return the
## cached data

##cacheSolve <- function(x, ...) {
##        ## Return a matrix that is the inverse of 'x'
##}
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrixinverse()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrixinverse(m)
    m
}