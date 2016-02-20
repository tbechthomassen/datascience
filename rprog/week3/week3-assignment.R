makeCacheMatrix <- function(input = matrix()) {
     raw_data <- function() {input} # is the function() really necessary? test later. maybe just foir uniform classes in list?
     cache <- function() {inv}
     old_mtrx <- function() {mtrx}
     list(raw_data = raw_data,
          cache = cache,
          old_mtrx = old_mtrx)
}

cachesolve <- function(input, ...){
     mtrx <- input$raw_data()
     # her skal der stå noget med en variabel som er den "gamle" matrix
     if(identical(old_mtrx, mtrx)){
          message("Inverted matrix and cache identical. Using cached matrix.")
          input$cache()
     } else if(!identical(old_mtrx, input$raw_data())){
          message("Matrix and cache not identical. Inverting matrix.")
          inv <<- solve(mtrx)
     } else if (is.null(mtrx)) {
          message("Matrix empty.")
     } else {}
     cache <- input$cache()
     cache
}

m 

n <- matrix(sample(1:10, 25, replace=TRUE), nrow=5, ncol=5, byrow=FALSE) 
test <- makeCacheMatrix(n)
testsolve <- cachesolve(test)
testsolve
