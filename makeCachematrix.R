##creates a matrix object that caches its inverse
makeCachematrix <- function(x = matric()){
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){x}
    setInverse <- function(inverse){inv<<- inverse}
    getInverse <- function(){inv}
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

##computes the inverse of the matrix returned by the makeCachematrix function
cacheSolve <- function(x, ...){
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
