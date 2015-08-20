## This code contains 2 functions that work to cache
## the inverse of a matrix

## This function creates a special "matrix" object that can
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL                                                              ## Initiate variable 1
  
  set <- function(y){                                                    ## Initiate function 1
    x <<- y                                                              ## Use <<- operator to
    m <<- NULL                                                           ## make assignment global
  }
  
  get <- function() x                                                    ## Initiate function 2
  setmatrix <- function(solve) m <<- solve                               ## Initiate function 3
  getmatrix <- function() m                                              ## Initiate function 4
  
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)       ## Return a list of four elements
  
}

## This function computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix" above

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()                                                    
  
  if(!is.null(m)){                                                      ## Return cached matrix if input is
    message("getting cached data")                                      ## a repeat
    return(m)
  }
  
  matrix <- x$get()                                                     ## If the input is new then,
  m <- solve(matrix, ...)                                               ## calculate the inverse and,
  x$setmatrix(m)                                                        ## set and return in m
  
  m                                                                     
  
}
