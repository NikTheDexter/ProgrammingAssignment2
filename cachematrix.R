## Put comments here that give an overall description of what your
## functions do
### makeCacheMatrix :
           #Input Parameters : This function accepts one matrix vector
		   #Core Logic : Once supplied with an invertible matrix this functions does below job :
		   #			 1- setmat : This function object accepts the matrix supplied by the user and initiates the inverse matrix at the same time
           #             2- getmat : This function object is used to show the accepted user defined matrix
           #             3- setmatinverse : This function object is used to perform the matrix inversion
           #                                It points to the cacheSolve funtion with the help of  <<- operator
           #             4- getmatinverse : This function object is used to show the inverted matrix outputed by standard "solve" function present in the cacheSolve function.

### cacheSolve :
           #Input Parameters : It just accepts the input matrix from the parent makeCacheMatrix function which is to be inverted 
		   #Core Logic : Once it accpets the matrix vector, it performs below steps :
		   #             1- It first check, whether there is already an inverse of provided matrix is present in R cache
		   #             2- If not present in cache it generate the inverse of matrix with the help of "solve" method which is standard function present in R to inverse the matrix
		   #             3- If it found the inverse of the matrix in cache, it simply retruns that inverse matrix to callinf function with the message saying that this inverse has been fetched from the cache
		   #             4- Else, it just retruns the inverse matrix which then stored to the getmatinverse object
 		   

### Testing :
           # Here is one matrix provided as parameter to get its inverse with the help of below steps :
		   #          x <- matrix(c(12,13,14,15),2,2)
           #           > z <- makeCacheMatrix(x)
           #           > cacheSolve(z)
           #                 [,1] [,2]
           #            [1,] -7.5    7
           #            [2,]  6.5   -6
           #           > cacheSolve(z)
           #             fetch the cached data...
           #                 [,1] [,2]
           #            [1,] -7.5    7
           #            [2,]  6.5   -6

##==================================================================================================================================================================================================================
		   
## Write a short comment describing this function
   # The main function which take the matrix vector and generate its inverse using cacheSolve method
   
makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  setmat <- function(c) {
          x <<- c            # Assign vector value to an object which may be outside the current enviornment 
          matinv <<- NULL      # Initialize the object which will contain the inverse of matrix 
  }
  getmat <- function() x
  setmatinverse <- function(getinverse) matinv <<- getinverse
  getmatinverse <- function() matinv
  list(setmat = setmat,
       getmat = getmat,
       setmatinverse = setmatinverse,
       getmatinverse = getmatinverse)
}


## Write a short comment describing this function
   # The child function which is used in the makeCacheMatrix function to generate Inverse of the given matrix.
   # It supplies the ouput in two ways,-
   #                1- Just by checking the earlier trace of inverse in R cache OR
   #                2- Generate the fresh copy of inverse and output it to its callee function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
		  
  #Check whether there is already inverse of provided matrix present
  invmat <- x$getmatinverse()
  
  # If present in cache, return it from cache
  if (!is.null(invmat)) {
          message("fetch the cached data...")
          return(invmat)
  }
  
  # Else , apply the "solve" method to get matrix inverse 
  data <- x$getmat()
  invmat <- solve(data, ...)
  x$setmatinverse(invmat)
  
  # Return inverse of given matrix
  invmat
}
