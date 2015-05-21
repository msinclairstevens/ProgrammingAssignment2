## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## 1. Set the value of the matrix to what is passed in, if anything.
  ##    Otherwise an empty matrix is created by default.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## 2. Get the value of the matrix set above.
  get <- function() x
  
  ## 3. Set the value of the inverse.
  # This is weird to me because you pass it a value. You never calculate it.
  setinverse <- function(inverse) m <<- inverse
  
  ## 4. Get the value of the inverse set above.
  getinverse <- function() m
  
  ## 5. Return a list with these function.
  # That way they can be called elsewhere.
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()    # Set m to the value of the getinverse column in x
  
  # Check to see if a value has been cached.
  # If so, return it.
  # If not, calculate the inverse of the matrix.
  if(!is.null(m)) {
    message("Getting cached data.")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  #uncachedMatrix <- x
  ## Return a matrix that is the inverse of 'x'
  #solve(uncachedMatrix)

}
