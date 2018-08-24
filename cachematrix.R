## makeCacheMatrix creates a special matrix and cacheSolve provies calculates inverse
## either cache it or retrives from special matrix

## makeCacheMatrix provies function to set or get matrix and set ot get inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    setmat <- function(y) {
      x <<- y
      inv <<- NULL
    }
    getmat <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(setmat = setmat, getmat = getmat,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve check the special matric for inverse value, if one is available
## then it retrives its for the special matrix, or creates the inverse and sets it to 
## the special matrix for future purpose(cacheing)

cacheSolve <- function(x) {
  
          inv <- x$getinv()
          if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          data <- x$getmat()
          inv <- solve(data)
          x$setinv(inv)
          inv
}

