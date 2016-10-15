#This is the function used for creating a special matrix object that can cache its inverse
#the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment.

makeCacheMatrix <- function(x = matrix()){
  mx <- NULL
  set <- function(y){
    x <<- y
    mx <<- NULL
  }
  #for setting the special matrix
  get <- function() x
  setInverse <- function(solve) mx<<- solve
  getInverse <- function() mx
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  
  mx <- x$getInverse()
  #First cached data is tried to be retrieved
  if(!is.null(mx)){
    message("getting the Cached Data....")
    return(mx)
  }
  dataOfMatrice <- x$get()
  mx <- solve(dataOfMatrice, ...)
  x$setInverse(mx)
  return(mx)
}