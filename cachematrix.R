## The objective of this project is to create two functions
## makeCacheMatrix and cacheSolve
## where the makeCacheMatrix function 
##creates a special "matrix" object that can 
## cache its inverse.
## makeCacheMatrix gets a matrix input x,
##return a list of unctions that set the matirx,get the matrix
## set the inverse and get the inverse
makeCacheMatrix <- function(x = matrix()) {
          Inv<-NULL
          ## set the inverse of the matrix =NULL
          set<-function(y){ 
            ##set takes an argument named y,which
             ## is a matrix
            x<<-y 
            ##<<-assigned values on the right side of the
            ##operator to an object in the parent environment 
            Inv<<-NULL
            ##null the value of Inv
          
          }
          get<-function()x 
          setInv<-function(Inverse)Inv<<-Inverse
          #Use <<-  to assign the input argument 
          ## to Inv in the parent environment
          getInv<-function()Inv 
          ##retrive the value of inv
          list(set=set,get=get,setInv=setInv,getInv=getInv)
          ##assins each of the functions 
          ##as an element within a list()
          ##i.e. give teh name set to set()function 
          ##give the name get to get() function...
}


## the cacheSolve funtion will calculate the inverse of the matrix x that is created
## by the makeVector function.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   Inv<-x$getInv()
  if(!is.null(Inv)){
    ##find out if the Inv of the matrix is null 
    ## if the Inv is not null, we have a cached Inverse and return it.
     message("getting cached data")
     return(Inv)
   }
   data<-x$get()
   ##if the Inv is null, cacheInv()gets the matrix from the input object(x)
   
   Inv<-solve(data,...)
   x$setInv(Inv)
   ##uses the setInves()function to set teh inv in the input object
   return(Inv)
   ##returns the values of the inverse to the parent environment.
}
