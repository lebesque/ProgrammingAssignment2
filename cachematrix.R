## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns the cached inverse matrix with original matrix. 
## Input variable type : Matrix
## Output : list of matrices

makeCacheMatrix <- function(x = matrix()) {
        ## sets the initial inverse matrix to NULL
        inv_x <- NULL
        
        ## For incoming new matrix, the original data is placed and the inverse is initialized.
        set <- function(y) {            
                x <<- y
                inv_x <<- NULL
        }
        
        ## Reads the original matrix
        get <- function() {
                x
        }
        
        ## Changes the inverse matrix
        setinv <- function(inv) {
                inv_x <<- inv
        }
        
        ## Reads the inverse matrix
        getinv <- function() {
                inv_x
        }
        
        ## Encapsulated matrices into a list with given variable names 
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse matrix.. 
## Input variable type : list from makeCacheMatrix
## Output : inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Reads the cached inverse matrix
        inv_x <- x$getinv()
        
        ## if already there, return the matrix
        if(!is.null(inv_x)) {
                message("getting cached inverse matrix of x")
                return(inv_x)
        }
        ## if not, calculate and return it
        else {        
                data <- x$get()
                inv_x <- solve(data)
                x$setinv(inv_x)
                return(inv_x)
        }

}
