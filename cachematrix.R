## set value of matrix
## get value of matrix
## set value of inverse matrix
## get value of inverse matrix


makeCacheMatrix <- function(x = matrix())  {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    getvalue <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, getvalue = getvalue, setinverse = setinverse, getinverse = getinverse)
    
}


## calculate inverse if inverse of matrix is never calculated before; otherwise, get it from cacheSolve

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$getvalue()
    i <- solve(data)
    x$setinverse(i)
    i
}


