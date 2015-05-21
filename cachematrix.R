## Put comments here that give an overall description of what your
## functions do.

  # 2015-05-20. Calculate the inverse of a matrix and cache the value.
  # Whenever you calculate the inverse of a matrix,
  #   first check to see if there is a cached value,
  #   if so, return the cached value rather than calculate it again.

## Write a short comment describing this function,
  # Create a list of four functions to store the value and inverse of a matrix.

makeCacheMatrix <- function(myMatrix = matrix()) {
  # myMatrix takes the value of a matrix. 
  # If none is passed when this function is called, default to an empty matrix.
  # Initialize the inverse as NULL. (Clear the cache.)
  myInverse <- NULL
  
  # 1. Create a function
  #    that resets the value of the myMatrix.
  #    If you call this function, it also resets the inverse to NULL,
  #    that is, it clears the cache.
  #    Note: This function is not called in the normal program flow.
  set <- function(aDifferentMatrix) {
    myMatrix <<- aDifferentMatrix
    myInverse <<- NULL
  }
  
  # 2. Create a function
  #     that returns the value of the matrix,
  #     either the matrix passed in when makeCacheMatrix was called,
  #     or the one passed in with the set() function.
  get <- function() myMatrix
  
  # 3. Create a function
  #     that stores the value of the inverse of myMatrix.
  #     This is the cache.
  setinverse <- function(inverse) myInverse <<- inverse
  
  ## 4. Get the value of the inverse set above.
  getinverse <- function() myInverse
  
  ## 5. Return a list with these function.
  # That way they can be called elsewhere.
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Write a short comment describing this function
  # Returns the inverse of a matrix.

cacheSolve <- function(x) {
  # Note: x is a list of functions run on a given matrix,
  #  not the matrix itself.
  # Check to see if the inverse of the matrix has already been
  #   calculated and cached.
  # If so, return the cached value.
  # If not, calculate the inverse of the matrix.
  
  # Get cached value for the inverse of the matrix.
  myInverse <- x$getinverse()    
  
  # If there is a cached value, return it.
  if(!is.null(myInverse)) {
    message("Getting cached data.")
    return(myInverse)
  }
  
  # If there is not a cached value,
  #   calculate the inverse of the matrix and cache the result.
  # 1. Get the matrix.
  data <- x$get()
  
  # 2. Calculate the inverse of the matrix.
  myInverse <- solve(data)
  
  # 3. Store the result of the calculation in the cache.
  x$setinverse(myInverse)
  
  # 4. Return the inverse of the matrix.
  myInverse
 
}
