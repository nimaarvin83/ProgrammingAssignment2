## makeCacheMatrix gets a matrix and shape a list consits of four arguments which will be fed into the cachemean function. 
## This function shape the input matrix to the cachemean function



makeCacheMatrix <- function(Main.Matrix = matrix()) {
  if (!is.matrix(Main.Matrix)|nrow(Main.Matrix)!=ncol(Main.Matrix)) {
    message("Please provide a square matrix !!!")
  } else {
    m <- NULL
    set <- function(y) {
      Main.Matrix <<- y
      m <<- NULL
    }
    get <- function() Main.Matrix
    setmean <- function(solve) m <<- solve
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
  }
  
}


## the cachemean function gets the list provided by the makeCacheMatrix function  
## if the list consits of the result (inverse of the matrix) it will return the cached value othervise it will calculate the inverse of the matrix
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}