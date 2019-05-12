## These two functions provide a means to calculated an inverse matrix from
## a given 2x2 matrix.  The inverse matrix is stored (cached) in the makeCacheMatrix
## object so that it doesn't need to be calculated each time other code may need
## to reference it.  We use R's lexical scoping to accomplish this and keep the
## getting/setting functions of the object in scope for the R session.

## varialble names: cm = current matrix
##                  ci = cached inverse matrix
##                  nm = new matrix
##                  ni = new inverse matrix
##                  CMO = cash matrix object


## the makeCacheMatrix function takes an incoming matrix and creates
## getter/setter function that maintain the state of the current matrix input
## and a calculated inverse of the input matrix.
## the function returns a list of these functions to the cacheSolve funtion
makeCacheMatrix <- function(cm = matrix()) 
{
  ci <- NULL
  
  get <- function() return(cm)
  getInverse <- function() return(ci)
  
  set <- function(nm) 
  {
    cm <<- nm
    ci <<- NULL
  }
  
  setInverse <- function(ni) { ci <<- ni}
  return(list(set=set, get=get, setInverse = setInverse, getInverse = getInverse))
}


## this function takes the list object returned from makeCacheMatrix
## and calls for the inverse matrix cached in the cache matrix object
## if the cached value exists it is returned.  if not an inverse matrix 
## is calculated and returned to the cache matrix object to insert into
## the cache variable (ci)
cacheSolve <- function(CMO, ...) 
{
  ci <- CMO$getInverse()
  
  if(!is.null(ci)) 
  {
    print('getting cached inverse matrix')
    return(ci)
  }
  
  data <- CMO$get()
  inverse <- solve(data)
  CMO$setInverse(inverse)
  print('solving for the inverse matrix')
  return(inverse)
}

## sample test matrices
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
