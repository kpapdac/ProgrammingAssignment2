## Function: makeCacheMatrix  -->creates a special "matrix" object that can cache its inverse.
## Function: cacheSolve  --> computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix takes the matrix x and returns a list of functions creating the "special" matrix. If the inverse of x was not
## calculated before it sets its value to NULL.

makeCacheMatrix <- function(x = matrix()) {
                inverse<-NULL
                set<- function(y){
                x<<- y
                inverse<<- NULL
  
                }
                get<- function()x
                setinv<- function(solve)inverse<<-solve
                getinv<-function()inverse
                list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve first looks for the "special" matrix of x in the output $getinv of function makeCacheMatrix. If the result is not NULL, then the 
## inverse of the matrix has been computed before and retrieves the result from previous calculation. Else, the inverse matrix is computed from the start using 
## the solve() function and it is stored in $setinv of makeCacheMatrix in case of future demand.

cacheSolve <- function(x, ...) {
              inverse<-x$getinv()
              if(!is.null(inverse)){
              message("getting cached data")
              return(inverse)
              }
              data<-x$get()
              inverse<-solve(data,...)
              x$setinv(inverse)
              inverse
}
