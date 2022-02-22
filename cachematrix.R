## The functions below allow the user to create a matrix, find its inverse,
##store it and then retrieve

## makeCacheMatrix creates a matrix, allows the user to get it, sets the
## inverse of the matrix and gets it (working in pair with cacheSolve function)

makeCacheMatrix <- function(m = matrix()) {
  s<-NULL
  set<-function(y){
    m<<-y
    s<<-NULL
  }
  get<-function() m
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function below allows the user to cache the inverse of the matrix 
## created earlier (with the help of makeCacheMatrix)

cacheSolve <- function(m, ...) {
  s <- m$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- m$get()
  s <- solve(data, ...)
  m$setsolve(s)
  s
}
