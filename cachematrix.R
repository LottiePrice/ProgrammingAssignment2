## makeCacheMatrix and cacheSolve are intended to
#reduce the computational load on repeated matrix inversions.
#Once the inverse has been calculated, it is cached 
#with the matrix and can be retrieved rather than 
#recomputed. 
#NOTE cacheSolve actually changes the object
#created by makeCacheMatrix: the inverse part gets changed.

#makeCacheMatrix is a list containing functions to
#get and set the matrix and its inverse. 
#get() and set() apply to the matrix itself;
#getInv() and setInv() get and set its inverse.
#Note that getInv() does not solve to find the inverse, it
#only stores an inverse that was found during the
#running of the cacheSolve function.

#Usage:
#m=matrix(c(1,2,3,4),2,2) #make a matrix
#v=makeCacheMatrix(m)     #insert it into the cacheMatrix
#v$get()                  #test that it's there
#cacheSolve(v)            #calculate the inverse
#v$getInv()               #Display the inverse
#cacheSolve(v)            #this iteration uses the cached inverse rather than recalculating it. 



makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInv <- function(inv) inverse <<- inv
    getInv <- function() inverse
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve takes a cacheMatrix object and calculates its inverse.
#The inverse is stored in the cacheMatrix object's inverse variable.
#If the inverse has already been calculated, cacheSolve returns the
#precalculated value rather than recalculating it.
#Usage:
#cacheSolve(v) #where v is the cached matrix created above

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    if(!is.null(x$getInv())){
        message("getting cached data")
        x$getInv()
    }
    else{
        message("calculating inverse")
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInv(inverse)
        inverse
    }
}
