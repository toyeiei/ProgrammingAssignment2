## There are two functiosn in this assignment
## first function create special matrix that will cache its inverse
## second function get cached result from the special matrix
## R code line 51-53 used to test these functions

## ---------------------------------------------------
## 
makeCacheMatrix <- function(x = matrix() ) {
  solve_mat <- NULL
        
  # set the value of matrix
  set <- function(y) {
    x <<- y
    solve_mat <<- NULL
  }     
  # get the value of matrix
  get <- function() x      
  # compute an inverse
  setSolve <- function(result) solve_mat <<- result     
  # get the inverse result
  getSolve <- function() solve_mat      
  # a list containing functions
  list(set = set, 
       get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## ---------------------------------------------------
## Return an inverse of a matrix
## Return cached inverse if a matrix has cached result

cacheSolve <- function(x, ...) {
  solve_mat <- x$getSolve()
        
  # check if solve_mat is NULL
  if( !is.null(solve_mat) ) {
    message("getting cached data")
    return(solve_mat)
  }
        
  # if NULL, do this process
  data <- x$get()
  solve_mat <- solve(data, ...)
  x$setSolve(solve_mat)
  return(solve_mat)
}

## ---------------------------------------------------
##  here is my testing code
myMat <- matrix(c(1,5,10,2,4,8,9,10,12), ncol=3)
hello <- makeCacheMatrix(myMat)
cacheSolve(hello)
