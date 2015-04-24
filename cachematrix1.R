## Assignment Coursera R Programming Lexical Scoping

makeCacheMatrix <- function(x = matrix()) {
  ## initialise m to NULL
  m<-NULL
  
  ## store a matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## returns the stored matrix
  get<-function() x
  
  ## cache the values in matrix
  setmatrix<-function(solve) m<<- solve
  
  ## get the cached values
  getmatrix<-function() m
  
  ## return a list
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## Function calculates inverse of makecachematrix from above
cacheSolve <- function(x=matrix(), ...) {
  
  ## get the cached value
  m<-x$getmatrix()
  
  ## if a cached value exists, return it
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## otherwise get the matrix, calculate the inverse, and store it in cache
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  
  ## return the inverse
  m
}
