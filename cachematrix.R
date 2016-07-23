## In this assignment example we make use of the <<- operator to assign a value to an object 
# in an environment that is different from the current environment. 

# Below are two functions makeCacheMatrix and cacheSolve that are used to 
# cache the inverse of a matrix.

# The first function, makeCacheMatrix will create a list, having a function to
# a.set the value of the matrix
# b.get the value of the matrix
# c.set the value of the inverse of the matrix
# d.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The cacheSolve function will return the inverse of the matrix.
# It will check if the inverse has already been computed.If not, 
# it will compute the inverse and set the value in the cache using setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data.")
    return(m)
  }
  data<- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
