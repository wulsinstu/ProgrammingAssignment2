## This function creates a special "matrix" object that can cache its inverse.
## If the cache objects don't exist, it first creates them
## Then it checks if the matrix is identical to the cached matrix. If so, it doesn't do anything new.
## If the new matrix is different, it is stored and its inverse is stored.

makeCacheMatrix <- function(x = matrix()) {
  if(!exists("cached_matrix")){
    cached_matrix <<- NULL
  }
  
  if(!exists("cached_inverse")){
    cached_inverse <<- NULL
  }
  
  if(identical(x, cached_matrix)){
    message("same")
  }
  else{
    cached_matrix <<- x
    cached_inverse <<- solve(x)
  }
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
## It first checks if the cache objects exist, and creates them if the don't.
## Then it returns the cached inverse if the new matrix matches the cached matrix.
## If the new matrix is different, it calculates the inverse, returns it, and stores it and the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(!exists("cached_matrix")){
    cached_matrix <<- NULL
  }
  
  if(!exists("cached_inverse")){
    cached_inverse <<- NULL
  }
  
  if(identical(x, cached_matrix)){
    message("getting cached data")
    return(cached_inverse)
  }
  else{
    cached_matrix <<- x
    cached_inverse <<- solve(x)
    return(cached_inverse)
  }
  
  
  
}
