## These two functions are used to generate and cache the inverse of a Matrix
## Computing the inverse of a matrix can be a time consuming operation
## Caching the value will make it faster to retrieve the result when required

## makeCacheMatrix function creates a pseudo object to store the Matrix and 
##   cache its inverse after it is calculated for the first time
## The input for this function is the matrix for which the inverse should be 
##   calculated and stored
## The output is a list of functions to be used in the matrix which work like
##   methods on an object
## The set function sets the matrix
## The get function returns the matrix
## The setinv function set the inverse of the matrix in the cache
## The getinv function gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the value of inverse to NULL 
    inverse <- NULL

    ## Create the set function which sets the matrix to x and resets the inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## The get function returns the matrix stored in this pseudo object
    get <- function() x

    ## The setinv function caches the inverse of the matrix in the inverse variable
    setinv <- function(inv) inverse <<- inv

    ## The getinv function gets the inverse of the matrix from the cache 
    getinv <- function() inverse
    
    ## Create (and output) a list of functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve function returns a matrix that is the inverse of the matrix
##   created by the makeCacheMatrix function
## The input is the list created by the makeCacheMatrix function
## The output is the inverse of the matrix 
##   If the inverse is stored in the cache that value will be retrieved
##     otherwise this function will calculate the inverse of the matrix using
##     the solve function and store its result in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ## Try to obtain the inverse of the matrix from the cache
  inv <- x$getinv()

  ## If the value returned is not null, meaning, the inverse is stored in the cache
  ##   the value of the cache is returned and the function is completed

  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## If the inverse of the matrix is not stored in the cache, it has to be calulated
  ## First retrieve the matrix using the get function from the makeCacheMatrix list
  ##   and store the result in the data variable
  data <- x$get()

  ## Calculate the inverse using the solve function. It assumes the matrix is invertable
  inv <- solve(data)

  ## Store the inverse result in the cache using the setinv function
  x$setinv(inv)

  ## Return the inverse of the matrix
  inv
}
