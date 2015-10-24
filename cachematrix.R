## This is a function that is a class for a matrix. on top of the matrix itself, it stores the inversion and 
## a change flag as well


makeCacheMatrix<- function(x = matrix())
{
  mat<- NULL
  changed<- TRUE
  set<-function(y)
  {
    x<<-y
    mat<<-NULL
    changed<<- TRUE
  }
  get<-function() x
  setinverse<- function(inverse)
  {
    mat<<-inverse
  } 
  getinverse<- function() mat
  getchanged<- function() changed
  setchanged<- function(c) changed<<- c
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse,setchanged=setchanged,getchanged=getchanged)
  
}

## This function does the inversion and also caches it

cacheSolve<- function(x, ...)
{
  inv<- x$getinverse()
  if(!is.null(inv) && x$getchanged() == FALSE)
  {
    message("getting cached data!")
    return(inv)
  }
  message("calculating")
  matrix<-x$get()
  inv<-solve(matrix)
  x$setinverse(inv)
  x$setchanged(FALSE)
  inv
}