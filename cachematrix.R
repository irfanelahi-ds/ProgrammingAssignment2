## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix vector and it accepts one matrix argument. This function
## returns a list of functions (set,get,set_inverse,get_inverse) which is used. These functions are defined
## within the scope of this function and it uses the concept of lexical scoping to emulate "caching" functionality.

## Write a short comment describing this function
## When a matrix is passed to this function as an argument, it creates a special matrix vector and returns
## a list of functions (set,get,set_inverse,get_inverse).

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix<-NULL 
  
  set<-function(mat_1i){
    x<<-mat_1i
    inverse_matrix<<-NULL
  }
  get<-function() x
  set_inverse<-function(inv_mat) inverse_matrix<<-inv_mat
  get_inverse<-function() inverse_matrix
  list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}



## Write a short comment describing this function
## This function is used to compute the inverse of the matrix which is passed as an argument to this function.
## The matrix passed as an argument to this function is the one created by preceding function.
## If the matrix's mean is new, it computes the inverse and stores/caches it via set_inverse function. Subsequent
## calls to cacheSolve function for the same matrix simply returns the stored inverse matrix and the need to compute
## the inverse in every function call is avoided thereby implementing the cache functionality. However, if 
## if new matrix vector is passed to this function, it nulls the previous cached value of inverse and computes newer
## inverse and caches it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverse_matrix<-x$get_inverse()
  
  if (!is.null(inverse_matrix)){
    message("getting cached data")
    return (inverse_matrix)
  }
  
  data_m<-x$get()
  inverse_matrix<-solve(data_m)
  x$set_inverse(inverse_matrix)
  inverse_matrix
}
