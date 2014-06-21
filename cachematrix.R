##In this assignment we will utilize two functions that cache the inverse of a matrix

##This function create the object of the matrix in which we can find the inverse
makeCacheMatrix <- function(m = matrix()){
  Inv <- NULL
  
  ## This sets the value of the matrix
  set <- function(x)  {
    m <<-    matrix
    Inv <<-  NULL
  }
  ## This gets the value of the matrix
  get <- function() {
    m
  }
  ## This sets the inverse of the matrix 
  setinverse <- function(inverse) {  
    INV<<- inverse
  }
  ## This gets the inverse of the matrix
  getinverse <- function()  {
    INV
  }
  ## This returns the list of the methods used 
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}
## The second function calculates the special matrix returned by the makeCacheMatrix
## in function 1.  This function will check to see if the inverse has already been 
## calculated.  If it has, it gets the inverse from the cache and skips the computation.  
## Otherwise, it calculates the inverse of the matrix and sets the inverse of the
## matrix in the cache via the setinverse function

cacheinverse <- function(m,...)  {
  ## This will get the cache value
  INV <- m$getinverse()
  ## If the the value is not null this will return the cache value
  if(!is.null(INV))  {
    message("getting cached data")
    return(INV)
  }
  
  ##If the value is null, this will calculate retrieve the matrix, calculate the inverse
  ##and store it in cache
  data <- m$get()
  INV <- solve(data)  %*% data
  
  ##return the inverse      
}