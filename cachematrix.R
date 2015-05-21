
## This is a set of functions to solve the inverse of a matrix.
## Since matrix inversion can be expensive, the inverse is cached.
## When you invoke the cacheSolve function, it checks the cache first.
## The cache is designed to reset itself if the matrix value changes.
##
## Here is an example of how to invoke these functions:
##  myMatrix <- (your matrix parms here)
##  a<-makeCacheMatrix()
##  a$set(myMatrix)
##  cacheSolve(a)

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  S <- NULL           #initialize the cached inverse
# if the Matrix value is set, reset the cached inverse
  set <- function(y) {
    x <<- y
    S <<- NULL
  }
  get <- function() x   # return the Matrix value
  setSolve <- function(Solve) S <<- Solve  #set the cached inverse
  getSolve <- function() S      #get the cached inverse
  list(set = set, get = get,    #set up the list of functions
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  S <- x$getSolve()     #try and find the inverse
  if(!is.null(S)) {     #check if we got one
    message("getting cached data")
    return(S)           #return what we got from the cache
  }
  #code from here down is only executed if the cached value was null
  data <- x$get()       #get the value for x so we can use it
  S <- solve(data, ...) #convert the inverse
  x$setSolve(S)         #save the inverse in the cache
  S                     #return the value we created, and cached
}
