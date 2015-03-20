##  This file contains a pair of functions, which are aimed at caching the inverse of a matrix. It can help us to avoid unnessarily recompution for a specific matirx. 


##  For the first function, the input is a matrix and the output four functions. Those four function are the fundation of the computation of second functions("cacheSolve").

makeCacheMatrix <- function(x = matrix()) {
in_m <- NULL                # initialize the value of inverse matrix.
    set <- function(y) {    # define set() for setting the value of in global environment
        x <<- y
        in_m <<- NULL
    }
    get <- function() x     # get() will appear in next function for assign the value of target matrix
    setinv <- function(inv_matrix) in_m <<- inv_matrix     # setinv() is to save the calculated inverse matrix into cache
    getinv <- function() in_m         # extract the inverse matrix from cache, even is NULL.
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The second function is used for caching the inverse matrix if it had calculated before, or calculating it if the caching value is NULL.
## The input of this funtion is the function "matrix" returned from the first function which is "set()", "get()", "setinv()", and "getinv()".

cacheSolve <- function(x = makeCacheMatrix(), ...) {        # the argument x should be written as makeCacheMatrix()
in_m <- x$getinv()                       # get the value from the first function
    if(!is.null(in_m) == TRUE) {         # if the in_m from cache is not NULL, return it.
        message("getting cached data")
        return(in_m)
    }
    data <- x$get()                      # if is NULL, then get the target matrix, and assign it in "data"
    in_m <- solve(data, ...)             # calculate it using "solve().
    x$setinv(in_m)                       # save it in cache 
    in_m                                 # return the inverse matrix
}
