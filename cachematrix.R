## Coursera Programming Assignment 2. 
## This script will contain 2 functions. 1st function can be used to 
## create an invertable Matrix. 2nd function be used to either return a
## cached inverted matrix or to invert the matrix and return it


## Create a matrix that can be inverted

makeCacheMatrix <- function(x = matrix()) {
      ## Intialise and "empty" object
      invMat <- NULL
      
      ## Set "value" of matrix
      set <- function(y) {
            x <<- y
            invMat <<- NULL
      }
      ## Return matrix "value"
      get <- function() x
      
      ## set the "value" of the inverse matrix
      setInverseMatrix <- function(inverseMatrix) invMat <<- inverseMatrix
      
      ## Return "value" of the inverse matrix
      getInverseMatrix <- function() invMat
      
      list(set = set, get = get,
           setInverseMatrix = setInverseMatrix,
           getInverseMatrix = getInverseMatrix)

}


## Invert a matrix or return a previous inverted matrix from cache

cacheSolve <- function(x, ...) {
      
      ## TRY Get the inverted matrix from x
      invMx <- x$getInverseMatrix()
      
      ## Check if the invert matrix exists then return it
      if(!is.null(invMx)) {
            message("Do not try and bend the spoon. That's impossible. Instead only try to realize the truth, that there is no spoon but THERE IS A MATRIX CACHED")
            return(invMx)
      }
      
      ## Initialise tempory matrix based on the previously created matrix
      tempMx <- x$get()
      ## Invert the matrix using the function solve()
      invMx <- solve(tempMx, ...)
      ## Assign the inverted matix to the matrixCache object
      x$setInverseMatrix(invMx)
      ## Return the inverted matrix
      invMx
      
}
