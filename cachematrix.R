## The below pair of functions cache the inverse of the matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(mat=matrix()){
  inv<-NULL
  set<-function(y){
    mat<<-y
    inv<<-NULL
  }
  get<-function() mat
  setinv<-function(mat_inv)inv<<-mat_inv
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache

cacheSolve<-function(x,...){
 inv<- x$getinv()
 if(!is.null(inv)){
   message("getting cached inverted matrix!")
   return(inv)
 }
 data<-x$get()  
 inv<-solve(data)
 x$setinv(inv)
 inv
}
