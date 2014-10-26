e two functions, makeCacheMatrix and cacheSolve, create a matrix and cache its inverse. 
## Then the cacheSolve retrieves the inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(origMtrx = matrix()) {
  
  ## Check if a valid matrix provided as input
  if(!is.matrix(origMtrx)){
    stop("Please give a matrix")
  }
  invMtrx <- NULL
  set <- function(y){
    origMtrx <<- y
    invMtrx <<- NULL
  }
  # Functions for getting and setting cached inv. matrix value
  get <- function() origMtrx
  # Inversing the matrix using build in solve() function in R
  setmatrix <- function(solve) invMtrx<<-solve
  getmatrix <- function() invMtrx
  list(
      set = set, 
      get = get, 
      setmatrix = setmatrix, 
      getmatrix = getmatrix
      )
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cache solve should retrieve the inverse from
## the cache.

cacheSolve <- function(origMtrx, ...) {
  ## Return a matrix that is the inverse of 'origMtrx'
  invMtrx <- origMtrx$getmatrix()
  # Check if we have cached matrix available?
  if(!is.null(invMtrx)){
    message("getting cached data")
    return(invMtrx)
  }
  ## If cached matrix not available then return that is the inverse of 'origMtrx'
  data <- origMtrx$get()
  invMtrx <- solve(data, ...)
  origMtrx$setmatrix(invMtrx)
  invMtrx
}
