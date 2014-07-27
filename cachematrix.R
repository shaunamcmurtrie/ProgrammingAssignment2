## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   invMatrix <- NULL
   set <- function(y) {
      x <<- y
      invMatrix <<- NULL
   }
   get <- function() x
   setInvMatrix <- function(im){
     invMatrix <<- im
     invMatrix <- im
   } 
   getInvMatrix <- function(){
     invMatrix
   }
   list(set = set, get = get,
    setInvMatrix = setInvMatrix,
    getInvMatrix = getInvMatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMtrx <- x$getInvMatrix()
  if(!is.null(iMtrx)){
    message("getting cached data")
    
    return(iMtrx)
  }
  data <- x$get()
  iMtrx <- solve(data)
  x$setInvMatrix(iMtrx)
  iMtrx
}
