## We write 2 functions: makeCacheMatrix to create a special object containing functions to access
## and set data of the matrix (the matrix itself and the inverse matrix)
## Second functions deals with the calculation of the inverse of the matrix. It receives the special 
## object created in makeCacheMatrix to access data and check if the inverse is already calculated or
## if the object is recently instantiated, it calculates the inverse and set that attribute in the object.

## makeCacheMatrix returns a list of functions to access and set data: the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinv<-function(inv) i<<-inv
  getinv<-function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve receives an object that contains a list of functions and its environment created when it is
## instantiated by makeCacheMatrix. It checks if the inverse is already calcualated, in that case, the function
## returns it directly. If not, the function calcultes it and set the value to the variable of the object
## that keeps the inverse matrix (i)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data)
  x$setinv(i)
  i
}
