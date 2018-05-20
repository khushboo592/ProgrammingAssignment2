## The "makeCacheMatrix" function creates a special array object, and then the "cacheSolve" function 
##calculates the inverse of the array. If the inverse of the array has already been computed, 
##it is found in the cache and returns it, with no need to recalculate it.

##makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)


makeCacheMatrix <- function(x = matrix()) {
                    inverse_x <- NULL
                    set <- function(y) {
                    x <<- y
                    inverse_x <<- NULL
                     }
              get <- function() x
              setinverse <- function(inverse) inverse_x <<- inverse
              getinverse <- function() inverse_x
              list(set = set,
              get = get,
              setinverse = setinverse ,
              getinverse = getinverse)

              }

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
                inverse_x <- x$getinverse()
                if(!is.null(inverse_x)) {
                message("Getting cached data.")
                return(inverse_x)
                   }
            data <- x$get()
            inverse_x <- solve(data)
            x$setinverse(inverse_x)
            inverse_x
            }

}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
