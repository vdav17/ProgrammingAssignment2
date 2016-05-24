## This functions takes advantage of the scoping rules available in R to cache
## the inverse of a Matrix in order to re use the inverse value if the same
## Matrix needs to be inverted more than once

## This function creates a list of functions:
## set : set the matrix that needs inversion
## get: get the matrix that needs inversion
## setinverse: set the cached value of the matrix inversion
## getinverse: get the cached value of the matrix inversion
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function receives a "special" matrix created by makeCacheMatrix
## It asks if the inverse is already calculated (not null), if it is it
## returns it from the cache, if it it is not, it calculates with solve and
## cache it. Asking if it is null is valid because everytime a new matrix is set
## in the special matrix, the cache inverse is set to null

cacheSolve <- function(x,...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
          message("***Getting Cached Data***")
          return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}

##You can use the following function to test it
##The examples were taken from: http://www.mathcentre.ac.uk/resources/uploaded/sigma-matrices7-2009-1.pdf
testCacheMatrixInversiontest<- function(){
  
  mat1 <- matrix( c(3,4,1,2), nrow=2, ncol=2)
  message("This is the first mat: ")
  print(mat1)
  
  specialmat1 <- makeCacheMatrix(mat1)
  invmat1 <- cacheSolve(specialmat1)
  message("This is the non cached inverse of the first mat:")
  print(invmat1)
  
  message("This is the cached inverse of the first mat:")
  invmat1 <- cacheSolve(specialmat1)
  print(invmat1)
  
  mat2 <- matrix( c(2,-3,4,1), nrow=2, ncol=2) 
  message("This is the second mat: ")
  print(mat2)
  specialmat2 <- makeCacheMatrix(mat2)
  invmat2 <- cacheSolve(specialmat2)
  message("This is the non cached inverse of the second mat:")
  print(invmat2)
  
  message("This is the cached inverse of the second mat:")
  invmat2 <- cacheSolve(specialmat2)
  print(invmat2)
}