## The Cachematrix functions allow to calcul the inverse of a matrix and saved the result
## in cache allowing to save time on second calcul while not calculing twice the same
## inverse matrix

## The makeCacheMatrix creates a matrix object which is able to save is own inverted matrix


makeCacheMatrix <- function(x = matrix()) {
  ##Initialisation of solve cached
  s <- NULL
  
  ##Method for setting matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ##Method for getting matrix
  get <- function() x
  
  ##Method for setting inversed matrix
  setsolve <- function(solve = matrix()) s <<- solve
  
  ##Method for getting inversed matrix
  getsolve <- function() s
  
  ##Computing element in a list
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## The cacheSolve function gets the inverted matrix of a matrix passed in parameters
## function determines if a cache matrix exists and if this one is correct.
## If not the function calculs it and save it in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  
  ##check if s exist
  if(!is.null(s))
  {
    return(s)
  }
  ##else calcul inverse and save it
  
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}
