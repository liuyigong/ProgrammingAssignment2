## Calculate the inverse of a inversable matrix. If such inverse has already 
## been calculated, it will retrieve the inverse from cache.

## Store four functions with input matrix to a list

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(i) inv <<- i
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Calculate the inverse if first time encounters, otherwise will retrieve from
## cache

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv        
}
