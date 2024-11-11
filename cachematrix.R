
### The first function (called makeCacheMatrix) creates a matrix object that can cache its inverse.
### 
### The second (called cacheSolve) function takes as argument the special "matrix" object produced by 
### the makeCacheMatrix function. It then returns the inverse of this object if the inverse is 
### already cached (with a message that says "getting cached data"), 
### or it calculates and returns the inverse if the inverse has not been already calculated and cached).


### The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) { #initialisation of the matrix object
    inverse <- NULL #initialisation of the inverse object
    
    #the following 4 line define the set() function: the superassignement operator is used 
    # to assign the input argument to the matrix object and to
    # assign NULL to the inverse object (both in the parent environment). 
    # Any previously cached values of inverse are cleared.
    set <- function(y) { 
        x <<- y
        inverse <<- NULL
    }
    get <- function() x #defines the getter for the matrix x
    setinverse <- function(inv_value) inverse <<- inv_value #defines the setter for the inverse inv_value
    getinverse <- function() inverse
    
    #the 4 functions defined previously are returned within a named list to the parent environment.
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


### The cacheSolve function takes as argument the special "matrix" object produced by 
### the makeCacheMatrix function.
### It then returns the inverse of this object if the inverse is already cached, 
### or it calculates and returns it if cache is empty.

cacheSolve <- function(makeCacheMatrix.object, ...) { #the function cacheSolve takes an input argument of type makeCacheMatrix.object
    ## The ellipsis (...) allows for additional arguments to be passed into the function cacheSolve.    
    ## Return a matrix that is the inverse of 'makeCacheMatrix.object'
    inverse.local <- makeCacheMatrix.object$getinverse() #calling the getinverse() function on the input object (makeCacheMatrix.object).
    if(!is.null(inverse.local)) { #checking to see whether the inverse value is not NULL (already calculated and cached). 
        message("getting cached data") #let the user know that there is already an inverse calculated & cached and will use it
        return(inverse.local) #exit the function , returning the cached inverse value
    }
    data <- makeCacheMatrix.object$get() #obtain the value of the matrix object
    inverse.local.calculated <- solve(data, ...) # compute the inverse of the matrix 
    makeCacheMatrix.object$setinverse(inverse.local.calculated) #use the setinverse() function on the input object to set the inverse in the input object
    inverse.local.calculated # return the inverse the value of the inverse to the parent environment
}


 
###############################################
#TEST CASES:
# Simple test matrices for the lexical scoping programming assignment
# from https://www.coursera.org/learn/r-programming/discussions/forums/_U6UVSj2EeaZ8Apto8QB_w/threads/ePlO1eMdEeahzg7_4P4Vvg

# I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2) #the identity matrix 2x2, the resultof the matrix multiplication between a 2x2 matrix and its inverse
# m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# m1_inv <- solve(m1) #the inverse is calculated "manually"

###############################################


