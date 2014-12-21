## This function creates a R object which initializes a variable mat.
## mat variable will be used to store inverse matrix, i.e cached data
## get() function obtains the new matrix for which inverse is to be computed.
## Function setMatrix() assigns computed inverse matrix to mat
## getMatrix() - To obtain the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setMatrix <- function(solve) mat <<- solve
        getMatrix <- function() mat
        list(set=set,get=get,setMatrix = setMatrix,getMatrix = getMatrix)
}


## This function finds the inverse of the matrix. 
## First it checks if the inverse already exists, if yes returns the inverse of the matrix and quits else,
## inverse of x is calculated , saved to cache and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getMatrix()
        if(!is.null(mat)){
                message("getting cache data")
                return(mat)
        }
        matrix <- x$get()
        mat <- solve(matrix, ...)
        x$setMatrix(mat)
        mat
}
