### solution for R-programming course-assignment week 2 ###


## makeCacheMatrix returns a list containing a number of functions for use in the  cacheSolve-function
makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      getInv <- function() i # gets the inverted function, i
      set <- function(inv) { i <<- inv } # takes an argument, and set it as i, making it available for env's set
      get <- function() x # gets the original input matrix
      list(original = get, getInverse = getInv, setInverse = set)
}


## cacheSolve returns either the inverts the matrix from makeCacheMatrix,
## or returns the cached inverted matrix, if the original matrix is unchanged.
cachesolve <- function(input, ...){
      
      if(!exists(x = "mtrx")){ # check if mtrx exists, if not, create empty variable "mtrx"
            mtrx <- NULL}
      
      if(identical(mtrx, test$original())){ # test if mtrx and original matrix are identical. If true, return cached matrix
            message("Matrix and cache identical. Using inverted matrix.")
            test$getInverse()
      } else if(!identical(mtrx, test$original())){ # test if mtrx & orginal matrix are identical, if not, invert matrix, cache & return it
            message("Matrix and cache not identical. Inverting matrix.")
            mtrx <- test$original()
            inverted_mtrx <- test$setInverse(solve(mtrx))
            test$getInverse()
      } else if (is.null(mtrx)) { # test if mtrx is empty, if true return an error message
            message("Matrix empty.")
      } else {message("Critcal Error. Is the matrix defined?")} #return an error message, if none of the above.
}

n <- matrix(sample(1:10, 25, replace=TRUE), nrow=5, ncol=5, byrow=FALSE) 
test <- makeCacheMatrix(n)
testsolve <- cachesolve(test)
testsolve



if(!exists(x = "mtrx")){
      mtrx <- NULL}
if(identical(mtrx, test$original())){
      message("Matrix and cache identical. Using inverted matrix.")
      test$getInverse()
} else if(!identical(mtrx, test$original())){
      message("Matrix and cache not identical. Inverting matrix.")
      mtrx <- test$original()
      inverted_mtrx <- test$setInverse(solve(mtrx))
      test$getInverse()
} else if (is.null(mtrx)) {
      message("Matrix empty.")
} else {message("Critcal Error. Is the matrix defined?")}

