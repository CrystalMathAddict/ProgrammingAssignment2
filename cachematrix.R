

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix: This function creates a special "matrix" object 
  ## that can cache its inverse. List is used in cacheSolve function.

  Inv<<- NULL
  set <- function(PriorX) {x <<- PriorX; Inv <<- NULL}    #Set value of matrix
  get <- function() x                                     #Get value of matrix
  setInv <- function(Inverse) Inv <-- Inverse             #Set value of inverse
  getInv <- function() Inv                                #Get value of inverse
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
          
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 ## This function computes the inverse of the special "matrix" returned by 
  ## makeCacheMatrix above. If the inverse has already been calculated 
  ## (and the matrix has not changed), then the cachesolve should 
  ## retrieve the inverse from the cache.
  
  
  Inv <- x$getInv()
  if(!is.null(Inv)) {return(Inv)}     #If Inv already calculated, will be returned
  
  data<- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  
  Inv
  
}
