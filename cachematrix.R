## This pair of functions cache the inverse of a given matrix. 
## We can set and/or get the matrix and its inverse using makeCacheMatrix().
## The inversed matrix will be returned using cacheSolve() from cached data
## if it is previously cached, otherwise the function will calculate and return it.

## makeCacheMatrix() creates a special matrix object "Inv"
## that can cache the inverse of given matrix "x".

makeCacheMatrix <- function(x = matrix()) { ## to input a matrix "x"
    Inv <- NULL ## "Inv" is created as empty space to store the inversed "x"
    set <- function(y) { ## to set a matrix to object created by makeCacheMatrix()
        x <<- y  ## to set "y" to "x" within the whole function using "<<-",
                 ## not only in the current environment
        Inv <<- NULL ## to initialize "Inv" to null within the whole function
                     ## after setting the new maxtix "y", we expect a new inverse
    }
    get <- function() x ## to return the input matrix "x"
    setInv <- function(Inverse) Inv <<- Inverse ## to set the inversed matrix
                                                ## and store it in "Inv"
    getInv <- function() Inv ## to return the inversed matrix "Inv"
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)  
    ## to return a list that contains these functions by names, for example:
    ## x <- makeCacheMatrix(test)
    ## x$set(NewMatrix) to change matrix
    ## x$get() to get the input matrix
    ## x$setInv(NewInversion) to set the inversed matrix
    ## x$getInv() to get the inversed matrix
}


## cacheSolve() calsulates the inverse of the special matrix returned by 
## makeCacheMatrix above. If the inverse has been calculated and cached, 
## cacheSolve() retrieves the inverse from the cache and return it.

cacheSolve <- function(x, ...) { ## to input the given matrix x
    Inv <- x$getInv() ## to get the inversed matrix from object x
                      ## and store it in "Inv", which will remain null if uncalculated
    if(!is.null(Inv)) { ## to check if the inverse matrix "Inv" is cached(stored)
        message("getting cached data") ## if so, show this message in ""
        return(Inv) ## and return the cached "Inv"
    }               ## Otherwise,
    data <- x$get() ## first to get the matrix object and store in "data"
    Inv <- solve(data, ...) ## then calculate the inverse of given matrix 
                            ## and store in "Inv"
    x$setInv(Inv)           ## then set it to the object
    return(Inv)             ## return the inversed result "Inv".
}