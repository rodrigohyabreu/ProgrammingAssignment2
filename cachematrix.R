## The code is composed by two functions: makeChacheMatrix and cacheSolve
## 
## makeCacheMatrix receives a matrix as argument and give attributes to it  
## using subfunctions 
## 
## cacheSolve receives the matrix adapted by the makeCacheMatrix and returns its
## inverse

###################################################################################

###################################################################################

## makeCacheMatrix receives a matrix as argument and give attributes to it  
## using subfunctions
## 
## each subfunction performs a routine that will be requested by the second 
## function cacheSolve
##

makeCacheMatrix <- function(x = matrix()) {

  # erase local variable 'm'
  m <- NULL
    
  # set the new values from the argument (erase global variable m)
  set <- function(y) {
      x <<- y
      m <<- NULL
    }
  
  # return the original matrix data
  get <- function() x
  
  # store the inverse into the memory (set global variable m)
  setinverse <- function(inverse) m <<- inverse
  
  # call the inverse stored from the memory (call global variable m)
  getinverse <- function() m
  
  # the adapted matrix is a list composed by attributes each representing a subfunction
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}
    
###################################################################################

###################################################################################

## cacheSolve receives a 'special'matrix (adapted by the makeCacheMatrix function)
## 
## it calculates the inverse of the 'special' matrix and has a special feature..
## 
## it stores the result into theglobal variable 'm'  
##
## therefore whenever requested, it returns the results from the memory and
## avoid recurrent (and unnecessary calculations)
##

cacheSolve <- function(x, ...) {

  # check if the inverse was already calculated
  m = x$getinverse()

  # if so, returns the result from the memory
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # if not, get the data from the makeCacheMatrix attribute
  data <- x$get()
  
  # calculates the inverse using the solve function
  m <- solve(data, ...)
  
  # store the result into the memory using makeCacheMatrix subfunction
  x$setinverse(m)
  
  # return the inverse
  m

}

###################################################################################
