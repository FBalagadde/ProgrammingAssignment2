## Caching the Inverse of a Matrix
## Below are two functions that store a matrix and cache its inverse
## Example:
## > matrix1 <- matrix(c(1, 2, 3, 0, 4, 5, 1, 0, 6), nrow = 3, ncol = 3, byrow = TRUE)
## > specialMatrix <- makeCacheMatrix(matrix1)
## > cacheSolve(specialMatrix)

## ## The function, makeCacheMatrix, creates a special "matrix" which is a list containing a function to
## 1.) set the value of the matrix  2.) get the value of the matrix 3.) set the value of the inverse 4.) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## The second function, cacheSolve, calculates the inverse of the special "matrix" created with the above function, makeCacheMatrix
## It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise it calculates the inverse Of the matrix and sets the value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached inverse matrix")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
