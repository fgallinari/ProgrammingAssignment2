## The following functions are used to cache squared matrix
## inversion. Avoiding costly unnecessary computation.

## makeCacheMatrix creates a special "matrix", which is really
## a list containing functions to:
## 1. set the data of the matrix
## 2. get the data of the matrix
## 3. set the data of the inverted matrix (solved)
## 4. get the data of the inverted matrix (solved)

makeCacheMatrix <- function(x = matrix()) {
  ## First time creating, set the solved as NULL
  s <- NULL
  
  ## setDate is used to update the data of the "matrix"
  ## set the solved back to NULL
  setData <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## return the data of the matrix
  getData <- function () x
  
  ## setSolved is used to set the data of the solved
  setSolved <- function (solved) s <<- solved
  
  ## return the data of the solved
  getSolved <- function () s
  
  ## list of possible functions to call
  list(setData = setData, 
       getData = getData,
       setSolved = setSolved,
       getSolved = getSolved)
}


## cacheSolve calculates the inverted matrix (solved) of
## the special matrix created with the above function.

cacheSolve <- function(x, ...) {
  ## Get the inverted matrix of 'x'
  s <- x$getSolved()
  
  ## Check to see if there is a cached value
  if (!is.null(s)) {
    ## If so, return the cached value that is the inverse of 'x'
    return(s)
  }
  
  ## Otherwise, calculate it for the first time
  data <- x$getData()
  s <- solve(data)
  
  ## And then, save it in the cache
  x$setSolved(s)
  
  ## Return a matrix that is the inverse of 'x'
  s
}
