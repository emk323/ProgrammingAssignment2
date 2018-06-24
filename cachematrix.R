## Put comments here that give an overall description of what your
## functions do

##This function takes a matrix that has been inversed, and stored or caches it in the parent environment
makeCacheMatrix <- function(x = matrix()) {
  i<-NULL #object within this that can be used later
  set <- function(y) { 
    x <<- y #this double arrow means that we want x to equal this function when we call it in the parent environment
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inversematrix) i <<-inversematrix 
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## this function uses the solve funtion which will inverse a matrix, but before , it checks to 
## see if an inverse has already been calculated and cached, if it is it will read getting cached data,
## if not the inverse will be calculated and stored in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve (data, ...)
  x$setinverse(i)
  i
}