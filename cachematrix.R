## The first function creates object that caches matrix inverse, the second computes inverse / gets it from cache

## makeCacheMatrix takes matrix as an argument, returns object that caches matrix inverse

makeCacheMatrix <- function(x = matrix()) {inv <- NULL
      set <- function(y){x <<- y
            		inv <<- NULL}
      get <- function() {x}
      setinverse <- function(inverse) {inv <<- inverse}
      getinverse <- function() {inv}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## to refer to the functions with the $
}


## cachesolve function takes matrix as an argument, returns its inverse (calculates or retrieves previously computed)

cacheSolve <- function(x, ...) {
      				inv <- x$getinverse()
      				if(!is.null(inv)){message("retrieved cache value")
            			return(inv)
      						}
      				matx <- x$get()
      				inv <- solve(matx, ...)
      				x$setinverse(inv)
      				inv
				}