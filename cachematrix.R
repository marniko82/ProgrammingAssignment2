## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

cacheSolve <- function(x, ...)  {
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

# rand_choice = rnorm(1:16000000)
# a <- makeCacheMatrix()
# a$set(matrix(rand_choice, nrow=sqrt(length(rand_choice))))          #set the matrix
# a$get                                        #get the matrix 
# cacheSolve(a)                        #calculate the inverse 
# cacheSolve(a)                        #when is called back use the cached inverse  