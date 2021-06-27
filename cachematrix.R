## SetInverse x as matrix
## and solved the value "j" as null

makeCacheMatrix <- function(x = matrix)(sample(1:100,9,3,3)) {
j <- NULL
set <- function(y){
  x<<-y
  j<<- NULL
}
get <-function()x
  setInverse <-function(inverse)j<<-inverse
getInverse<-function(j)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Changed "m" to "j"

cacheSolve <- function(x, ...) {
  j<-x$getInverse()
  if(!is.null(j)){
    message("getting cache data")
    return(j)
  }
  data<-x$get()
  j<-solve(data, ...)
  x$setInverse(j)
  j
}

inv<-NULL
set<-function(y){
  x<<-y
  inv<<-NULL
}
get<-function()x
setinv<-function(inverse) inv<<-inverse
getinv<-function()inv
list(set=set,get=get,setinv=setinv,getinv=getinv)

j<-matrixrnorm(16(44))
cachesolve(j1)
[,1][,2][,3][,4]

}