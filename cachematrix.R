## https://github.com/DenysV/R_programming_assignment_2/blob/master/cachematrix.R
## 1st commit SHA-1 hash identifier: 8f6d5516df1428066563748eb022be83c680d15d
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix",
## which is really a list containing a function to
## set_Matrix - set the value of the matrix
## get_Matrix - get the value of the matrix
## set_Inverse - set the value of the inverse of the matrix
## get_Inverse - get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix){
  rix <- NULL
  set_Matrix <- function(New_Value){
    x <<- New_Value
    rix <<- NULL
  }
  get_Matrix <- function() {x}
  set_Inverse <- function(inverse){rix <- inverse}
  get_Inverse <- function(){rix}
  list(set_Matrix = set_Matrix, 
       get_Matrix = get_Matrix, 
       set_Inverse = set_Inverse, 
       get_Inverse = get_Inverse)
}
## The cacheSolve function calculates the inverse of the special "matrix"
## the special "matrix" which created with the makeCacheMatrix function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse
## in the cache via the setinverse function.
casheSolve <- function(x, ...){
  rix <- x$get_Inverse()
  if(!is.null(rix)){
    message("To get Cached data")
    return(rix)
  }
  data <- x$get_Matrix()
  rix <- solve(data)
  x$set_Inverse(rix)
  rix
}  
