## These two functions together allow us save in the cache the inverse of a matrix
## so that we don't have to compute it more than once in case that we need
## to compute it many times. This saves time because computing matrices' inverses
## is time consuming.

## This first function creates an environment with four functions and two objects,
## x and inv.

makeCacheMatrix <- function(X = matrix()) {
  inv<-NULL
  set<-function(Y){
    X<<-Y
    inv<<-NULL
  }
  get<-function() X
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This second function receives an object of the form makeCacheMatrix,
## checks if the corresponding inverse matrix is in cache, and in that case,
## returns the inverse matrix stored in the cache. In the other case,
## it computes the inverse matrix and returns it.

cacheSolve <- function(X, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-X$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<-X$get()
  inv<-solve(data, ...)
  X$setinverse(inv)
  inv
}
