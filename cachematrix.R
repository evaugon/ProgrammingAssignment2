## Below you will find a pair of functions that compute the inverse of a matrix and also 

## The first function, makeCacheMatrix will set the matrix, get the matrix, 
#set the inverse of the matrix and get the cached inverse of the matrix

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setInv <- function(inv) inverse <<- inv;
    getInv <- function() return(inverse);
    return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## The function below calculates the inverse of a matrix. What is special of this 
## matrix is that if the inverse is already cached, it retrieves it. Otherwise, it 
## calculates the inverse, caches it and returns it. 

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getInv()
    if(!is.null(inverse)) {
        message("Getting cached inverse ... Very fast!")
        return(inverse)
    }
    data <- mtx$get()
    inverse <- solve(data, ...)
    mtx$setInv(inverse)
    return(inverse)
}
