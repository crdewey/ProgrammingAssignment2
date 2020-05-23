## The functions return the inverse of a matrix
##To run:
## source("cacheSolve.R")
## create a vector using makeCacheMatrix for the matrix you want to find the inverse, e.g.
## aV <- makeCacheMatrix(matrix(c(-3,5,1,0), nrow=2, ncol=2))
## then run cacheSolve function for the vector created e.g.
## cacheSolve(aV)

## makeCacheMatrix function initialises the input matrix and clears the inverse matrix 
## for the new calculation.  It gets the matrix from and returns the inverse to the 
## parent environment and creates a list of named objects for use in cacheSolve function
makeCacheMatrix <- function(x = numeric()) {
  # initialise an object inv 
  inv <- NULL
  #define a function called set
  set <- function(y) {
    #assign an argument to the object x in the parent environment
    x <<- y
    #assign the value NULL to the object inv in the parent environment
    #to clear it for the new calculation
    inv <<- NULL
  }
  #retrieve x from the parent environment
  get <- function() x
  #assign the result into inv object in parent environment after setinver
  #funtion completes
  setinver <- function(inver) inv <<- inver
  #retrieve inv from parent environment
  getinver <- function() inv
  #create a list of objects with names and returns it to parent environment
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## cacheSolve returns the inverse matrix or x either from cache or by solving it 
cacheSolve <- function(x, ...) {
  # call the getinver function
  inv <- x$getinver()
  # if inv is not null then there is a result in cache to retrieve and return
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if inv is null the inverse is found, assigned to inv and returned
  data <- x$get()
  inv <- solve(data, ...)
  x$setinver(inv)
  inv
}


