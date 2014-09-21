## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix(1:4,2,2))
{
  m <- NULL # m is NULL
  set <- function(y) 
  {
    #print(x)
    m <<- NULL
    x <<- y
    #print(x)
  }
  get <- function() x
  #   calmean <- function () {
  #     m <<- mean(x)
  #     print(mean)
  #   }
  setinverse <- function(inverse)
  {
    m<<-inverse
  }  
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
}
