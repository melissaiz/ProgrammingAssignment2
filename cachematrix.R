## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly

## The following two functions are used to create a matrix object that can cache its inverse,
## then compute the inverse of matrix. If the inverse has already been calculated, then
## the inverse will be retrieved from the cache instead of being recomputed.

## This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Create a matrix that can cache its inverse.
  ## Returns a list containing four functions:
  ## 1. Create the matrix
  ## 2. Get the value of the matrix
  ## 3. Set the value of inverse of the matrix
  ## 4. Get the value of inverse of the matrix
  
  inv <- NULL #instantiate the inverse with NULL value
  set <- function(y) { #set function stores the matrix (x) and the NULL inverse in another environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse #calculate and set the inverse of the matrix
  getinverse <- function() inv #get the value of the inverse of the matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) #return list of 4 functions
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse() #check if the inverse has been computed and cached already
  if(!is.null(inv)) { #if the inverse has been cached, notify and return the cached computation
    message("getting cached data.")
    return(inv)
  }
  #the inverse has not been cached, so compute and return it now
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

