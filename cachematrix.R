#Create the "object" for storing Matrix and its cached inverse (if already calculated)
#The formal parameter x" and "inv" are objects, while set, get, setinverse, getinverse are "methods"

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #set is usually not needed when instantiating for the first time.
  #That's because the same operations are done at the time of function invocation.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #return x
  get <- function() {
    x
  }
  
  #store the inverse. This function is to be called by the complementary function cacheSolve.
  #cacheSolve will set the inverse value after calculating it for the first time
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  #return the inverse. If the objec returned in not-null, we'll have saved an additional computation
  getinverse <- function() {
    inv
  }
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#...indicates that the function can accept additional parameters
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

#Main function:
x = rbind(c(1, 2), c(3, 4))
m = makeCacheMatrix(x)
m$get()

#Call function:
cacheSolve(m)
cacheSolve(m)
