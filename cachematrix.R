## Computing the inverse of large matrices is computer intensive. 
## For those cases where we are going to reuse the matrix inverse, 
## we have written a program that caches the inverse of a given matrix globally 
## instead of doing a new calculation each time it is needed. To save the solution, 
## we use R Lexical scoping rules to save the inverse so we can reuse when needed.





## Function makeCacheMatrix do things like: 
## Take the the orginal matrix as input
## Creates an special matrix of type list. Purpose is to store the inverse of a matrix
##  calculated in the other function, cachemean()

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}





## When call function cacheSolve it will see if the inverse has been calculated before. 
## If not it will calculate the matrix inverse with help of embedded function "solve" in R ,
##  store it in the cache with help of function setinverse.  
## If the inverse  been calculated before it will get it from cache and skip the further calculation

cacheSolve <- function(x, ...) {                     ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}                                    


## I have conducted a test run of the program by calling program as instructed by Gregory D. Horne 
## at the discussion forum with solid matrix as input instead of Vector. 
## The test run, the outcome in a positive way