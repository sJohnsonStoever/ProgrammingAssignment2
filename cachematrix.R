## makeCacheMatrix and cacheSolve
## 
## When dealing with large matrices, obtaining the matrix inverse
## can be an expensive calculation. In order to reduce the burden 
## on the end user, the CacheMatrix data structure is introduced 
## which stores a matrix and its inverse. This inverse, however, 
## is not stored until it is explicitly calculated for the first
## time, which allows for minimal overhead over the natural matrix 
## object should the inverse not be needed. 
##


## makeCacheMatrix(x) : Given an invertible matrix x we construct a
##      CacheMatrixdata structure corresponding to the input matrix. This
##      is referred to as the "main" matrix being held by the CacheMatrix.
##
## One can interact with objects created via makeCacheMatrix using:
##
## $set() : Change the "main" (invertible) matrix being held within  
##      the CacheMatrixby passing in another matrix with this function.
##      NOTE: This will also clear the matrix inverse, if any, 
##      being stored by the CacheMatrix.
##
## $get() : Returns the "main" matrix being stored in the CacheMatrix
##
## $setinverse() : Stores a matrix into the CacheMatrix that 
##      represents the inverse of the "main" matrix being held.
##
## $getinverse() : returns the matrix that corresponds to the "main"
##      matrix being held by the Cachematrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        # Clear the matrix inverse since the matrix has changed:
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    # Data structure is created, return list pointing
    # to data structure functions
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve(x) : Given a CacheMatrix x as input, return the inverse
##      of the "main" matrix stored within that object. If an inverse
##      matrix is already stored within that object, return that matrix.
##      If no inverse is stored yet, compute the inverse and store that
##      matrix back within the CacheMatrix. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        # Inverse has been computed already.
        # So return that matrix:
        return(inv)
    }
    # Compute the matrix inverse
    mat <- x$get()
    inv <- solve(mat)
    # Save the computed inverse into the CacheMatrix
    x$setinv(inv)
    # Return the computed inverse
    inv
}
