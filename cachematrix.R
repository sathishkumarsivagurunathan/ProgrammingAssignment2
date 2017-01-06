## First function "makeCacheMatrix" has ability to 
## 1) Cache the inverse of the matrix
## 2) Calculate the inverse of the matrix using solve command.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Second function has ability do the following.
## 1) Get the inverse of the matrix object x.
## 2) If it is found, it returns the inverse of the object.
## 3) Else it gets the new matrix object and calculates its inverse and returns to the user.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
        message("getting cached data")
        return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m        
}
