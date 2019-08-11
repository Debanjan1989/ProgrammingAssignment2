## The below pair of functions cache the inverse of the matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(mat=matrix()){
  inv<-NULL
 
 ##defining the set function 
  set<-function(y){
    mat<<-y
    inv<<-NULL
  }
  
  ##function the extract the set value
  get<-function() mat
##function setting the value of inverse of defined matrix
  setinv<-function(mat_inv)inv<<-mat_inv
##Function to return the inverted matrix  
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache

cacheSolve<-function(x,...){
##extracting the inverted matrix   
 inv<- x$getinv()
  
 ##if value is present we will return the value and exit the function 
 if(!is.null(inv)){
   message("getting cached inverted matrix!")
   return(inv)
 }
 ##else extract the data and invert it 
 data<-x$get()  
 inv<-solve(data)
##setting the value of inverted data  
 x$setinv(inv)
 inv
}
