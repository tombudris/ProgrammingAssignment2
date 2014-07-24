## The functions below provide a means to instantiate a special "matrix-like"
## object (which has the capability to store a cached inverse of itself) and 
## another to calculate the inverse of one of these "matrix-like" objects, 
## exploit a cached inverse (if one exists), or populate a cached inverse (if 
## one does not exist).  No error handling here for singular input matrices!

## --------------------------------------------------------------------------
## Function 'makeCacheMatrix()' creates a special "matrix", which is
## actually a list containing a function to:
##      1.  set the values of the matrix
##      2.  get the values of the matrix
##      3.  set the values of the inverse of the matrix
##      4.  get the values of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # default initialization: set matrix inverse 'minv' variable to NULL
        minv <- NULL
        
        # 'set' function: pass input 'y' up scope as the new matrix 'x'
        # and reset the minv to NULL since the matrix must have changed
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        
        # 'get' function: return matrix 'x'
        get <- function() x
        
        # 'setinv' function: solve inverse and store it in 'minv'
        setinv <- function(solve) minv <<- solve
        
        # 'getinv' function: return 'minv'
        getinv <- function() minv
        
        # default output: the list of makeCacheMatrix() functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)       
}

## --------------------------------------------------------------------------
## 'cacheSolve()' calculates the inverse of the specialized "matrix" created
## by the function 'makeCacheMatrix()', which exploits a pre-computed inverse
## (cached in the matrix-like object) if one exists.  Otherwise, the inverse
## is calculated here, and cached onto the matrix-like object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        minv <- x$getinv()
        # if there's an inverse already solved (i.e. cache is not NULL), use it...
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        
        # ...otherwise, we need to compute the inverse right here and return it
        data <- x$get()
        minv <- solve(data, ...)
        x$setinv(minv)
        minv        
}
