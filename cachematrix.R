## Programming Assignment 2 - R programming (data science coursera)
## The following two functions are used to create a special object that stores 
## a matrix and caches its Inverse.


## The function makeCacheMAtrix creates a special "vector", 
## which is really a list containing a function to
## 1. set the  matrix == set()
## 2. get the matrix == get()
## 3. set the inverse of the matrix == setInverse()
## 4. get the inverse of the matrix == getInverse()


makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      
      get <- function() x
      
      setInverse <- function(inverse) inv <<- inverse
      
      getInverse <- function() inv
  
      list(set = set, get = get, 
           setInverse = setInverse, getInverse=getInverse)

}

## The following function computes the inverse of a matrix with caching.
## It first checks whether the inverse of a matrix is present in the cache.
## If yes, it simply returns the inverse value from cache. Otherwise, 
## it computes the inverse of matrix and stores it in the cache 
## and then returns the result to the caller. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)        
  }
  
  matrx <- x$get()
  inv <- solve(matrx, ...)
  x$setInverse(inv)
  inv
}