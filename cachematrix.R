## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
 # creates a special "matrix" object that can cache its inverse
 # assume that the matrix supplied is always invertible. 

    	m <- NULL

	#set the value of the matrix
  	setm <- function(y) 
  	
  	{

       		x <<- y

       		m <<- NULL

   	}
	#get the value of the matrix
   	getm <- function() x
   	
   	#set the inverse of the matrix

   	setsolve <- function(solve) m <<- solve
   	
	#get the inverse of the matrix
  	getsolve <- function() m

  	list(setm = setm, getm = getm,

       		setsolve = setsolve,

       		getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        # computes the inverse of the special "matrix" returned by makeCacheMatrix.
	   
	   
	   #get the inverse of the matrix from makeCacheMatrix
	    m <- x$getsolve()
	#if the inverse has already been calculated, then should retrieve the inverse from the cache
	    if(!is.null(m)) 
	    {
	
	        message("getting cached data")
	
	        return(m)
	
	    }
	
	    data <- x$getm()
	
	   m <- solve(data, ...)
	
	    x$setsolve(m)
	
	    m

}


#Testing the time difference between cache and real calculation time
#### mmm<-matrix(rnorm(1:1000000, 1), 1000, 1000)
#### ttt<-makeCacheMatrix(mmm)
####system.time(cacheSolve(ttt))
   #user  system elapsed 
   #1.86    0.01    1.88 
#### system.time(cacheSolve(ttt))
    #getting cached data
   #user  system elapsed 
    #  0       0       0 

