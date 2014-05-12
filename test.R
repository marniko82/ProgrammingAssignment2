makeCacheMatrix <- function(x = numeric()) {
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

# X <- matrix(rnorm(1600), nrow=sqrt(1600))
rand_choice = rnorm(1:1600)
a <- makeCacheMatrix()
a$set(matrix(rand_choice, nrow=sqrt(length(rand_choice))))          #set the vector
a$get                                        #get the vector 
cacheSolve(a)                        #calculate the mean 
cacheSolve(a)                        #when is called back use the cached mean  
