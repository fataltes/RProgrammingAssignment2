## first function makes a special type of matrix which has extra functions
## second function gets this special matrix as an argument and 
## creates its inverse if this matrix did not have an inverse previously


## This function is a special kind of matrix
## it actually has a hiddent matrix in it 
## and returns a list of four functions which do actions on this hidden matrix
## 1. get which returns the value of matrix
## 2. set which sets the value of matrix
## 3. getInverse which returns the inverse of matrix
## 4. setInverse which sets the inverse of matrix

## in set function each time a new matrix is set,
## value of inv will be set to NULL
## it is a sign for the matrix that is a new one different from the last matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv

  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## this function gets a function of type makeCacheMatrix as an argument
## if x has an inverse it returns the inverse
## else (when x has no inverse) it computes the inverse for the x
## sets it for x 
## and returns it as the inverse of the special matrix x
cacheSolve <- function(x, ...) {
  if (!is.null(x$getInverse())) {
    message("getting cache data")
    return(x$getInverse())
  }
  x$setInverse(solve(x$get()))
  x$getInverse()
}
