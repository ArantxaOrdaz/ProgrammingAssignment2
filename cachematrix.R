## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the inverse
# 4) get the inverse

makeCacheMatrix <- function(x = matrix()) {
INVERSE<-NULL
  set<-function(y){
       x<<-y
      INVERSE<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) INVERSE<<-inverse
  getinverse<-function()INVERSE
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function

# The following function calculates the inverse of the special "matrix" created with the
# above funtion. However, it first checks to see if the inverse has already been 
# calculated. If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse
# in the cache via the seatinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        INVERSE<-x$getinverse()
  if(!is.null(INVERSE)){
    message ("getting cached data")
    return(INVERSE)
  }
  data<-x$get()
  INVERSE<-solve(data,...)
  x$setinverse(INVERSE)
  INVERSE
}
