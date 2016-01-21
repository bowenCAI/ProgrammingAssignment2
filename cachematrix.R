## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverseM<-NULL
    #set the value of the matrix
    set<-function(y){
        x<<-y
        inverseM<<-NULL
    }
    #get the value of the matrix
    get<-function() x
    #set the value of inverse of the matrix
    setinverse<-function(inverse) inverseM<<-inverse
    #get the value of inverse of the matrix
    getinverse<-function() inverseM
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverseM<-x$getinverse()
    if(!is.null(inverseM)){
        message("getting cached data")
        return(inverseM)
    }
    data<-x$get()
    inverseM<-solve(data,...)
    x$setinverse(inverseM)
    inverseM
}

#testing
x<- rbind(c(1,2),c(2,1))
m<-makeCacheMatrix(x)
m$get()#display the matrix
m$getinverse()#get the inverse matrix
cacheSolve(m)#display the cached data
