## 1 - Write a function makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

## This function gets a matrix as an input, sets the value of the matrix, 
## gets the value of the matrix, sets the inverse matrix and gets the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {                ## defines the function with default mode of "matrix"
    InvMatrix <- NULL                                      ## initializes the inverse property (InvMatrix) as NULL
    SetMatrix <- function(y) {                             ## defines the SetMatrix function to assign new matrix
        x <<- y                                            ## assigns value of matrix in parent environment
        InvMatrix <<- NULL                                 ## resets InvMatrix to NULL
    }
    GetMatrix <- function() x                              ## defines GetMatrix function to return the value of matrix
    SetInverse <- function(inverse) InvMatrix <<- inverse  ## sets and assigns the value of the inverse matrix
    GetInverse <- function() InvMatrix                     ## gets the value of the inverse matrix
    list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, 
         SetInverse = SetInverse, GetInverse = GetInverse) ## returns a list of the methods
}


## 2 - Write a function cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    InvMatrix <- x$GetInverse()                            ## returns a matrix that is the inverse of 'x'
    if(!is.null(InvMatrix)) {                              ## returns the inverse if it is already set
        message("Getting the cached data...")
        return(InvMatrix)
    }
    DataMatrix <- x$GetMatrix()                            ## gets the original matrix data
    InvMatrix <- solve(DataMatrix, ...)                    ## calculates the inverse
    x$SetInverse(InvMatrix)                                ## sets the inverse matrix
    InvMatrix                                              ## returns the inverse matrix
}