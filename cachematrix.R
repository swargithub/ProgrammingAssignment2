## The following two functions are used to find the inverse of a matrix and store it in cache and when retrieving it 
## check the cache first and only if not found in cache get the inverse.

## The function makeCacheMatrix finds the inverse of a matrix by calling the solve function. 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      r <- nrow(x)
      identitymatrix <- diag(r)
        
      set <- function(y) {
        x <<- y
        i <<- NULL
      } 
      
      get <- function() x
      
      setinverse <- function(solve) i <<- solve(x,identitymatrix)
      
      getinverse <- function() i
      
      list( set = set, get = get, setinverse =setinverse, getinverse = getinverse)
}


## Cache the inverse of a matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
       i <- x$getinverse()
       
       if(!is.null(i)){
         message ("getting cached data")
         return(i)
       }
       
       data <- x$get()
       i <- solve(data, ...)
       x$setinverse(i)
       i
}