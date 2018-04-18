

## This function mean to get a special matrix that is able to cache the inverse result of itself

makeCacheMatrix <- function(value = matrix()) {
  solvedmatrix <- NULL
  set <- function(y){
    value <<- y
    solvedmatrix <<- NULL
    
  }
  get <- function() value
  setsolution <- function(s) solvedmatrix <<- s
  getsolution <- function() solvedmatrix
  list(set=set,get=get,setsolution=setsolution,getsolution=getsolution)
  

}


## This function mean to get inverse(solution) of a matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  solvedmatrix <- x$getsolution()
  if(!is.null(solvedmatrix)){
    message("getting cached solution")
    return(solvedmatrix)
  }
  mdata <- x$get()
  solution <- solve(mdata)
  x$setsolution(solution)
  solution
}


### test example

datatest = matrix(c(3,3.2,3.5,3.6),nrow=2)
datatest
solve(datatest)
specialm = makeCacheMatrix(datatest)
cacheSolve(specialm)
cacheSolve(specialm)