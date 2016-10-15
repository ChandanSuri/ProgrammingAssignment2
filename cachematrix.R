makeCacheMatrix <- function(x = matrix()){
  mx <- NULL
  set <- function(y){
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mx<<- solve
  getInverse <- function() mx
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  
  mx <- x$getInverse()
  if(!is.null(mx)){
    message("getting the Cached Data....")
    return(mx)
  }
  dataOfMatrice <- x$get()
  mx <- solve(dataOfMatrice, ...)
  x$setInverse(mx)
  mx
}