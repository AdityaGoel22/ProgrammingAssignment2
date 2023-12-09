## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)
nakeCacheMarix <- function(x= matrix()){
  inv<- NULL
  set<- function(y){
    x<-y
    inv<-NULL
  }
  get<- function()x
  setinv <- function(inverse)inv<<-inverse
  getinv<-function(){
    inver<- ginv(x)
    inver%*%x
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <-function(x, ...){
  inv<-x$getinv()
  if(!is.null(inv)){
    message("Getting Cache Data")
    return(inv)
  }
  datad<- x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}

f<- nakeCacheMarix(matrix(1:8,2,4))
f$get()
f$getinv()
cacheSolve(f)