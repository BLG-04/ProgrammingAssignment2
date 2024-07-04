
 # makeCacheMatrix: This function creates
 # a special "matrix" object that can cache its inverse.
 # cacheSolve: This function computes the inverse of the
 # special "matrix" returned by makeCacheMatrix above.
 # If the inverse has already been calculated
 # (and the matrix has not changed), then cacheSolve
 # should retrieve the inverse from the cache.
 #
 # -----------------------------------------
 # create a list containing a functions to:
 # 1. set the matrix ('setMatrix').
 # 2. get the matrix ('getMatrix').
 # 3. set the inverse matrix ('setInverse').
 # 4. get the inverse matrix ('getInverse').
 
 makeCacheMatrix <- function(x = matrix()) {
     
     # create a vector to hold the inverse.
     i <- NULL
     
     # overrides 'matrix' with input 'y' and resets 'i'.
     setMatrix <- function(y) {
         x <<- y
         i <<- NULL
     }
     
     # retrieves matrix.
     getMatrix <- function() x
     
     # override inverse manually stored in 'i'.
     setInverse <- function(inverse) i <<- inverse
     
     # retrieves inverse stored in 'i'.
     getInverse <- function() i
     
     # creates a list to access each function stored
     # within makeCacheMatrix function.
     list(setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
 }
 
 # create cacheSolve to calculate the inverse and store it.
 cacheSolve <- function(x, ...) {
     
     # get cached inverse.
     inverse <- x$getInverse()
     
     if(!is.null(inverse)) {
         # prints message if data is found.
         message("getting cached data")
         return(inverse)
     }
     
     # if cached inverse returns NULL...
     # loads matrix in vector 'data' and solves the inverse.
     data <- x$getMatrix()
     inverse <- solve(data, ...)
     
     # caches inverse.
     x$setInverse(inverse)
     inverse
 }
 # -------------test--------------
 # create a matrix
 test <- makeCacheMatrix(matrix(c(10,20,30,40,50,60,70,80,90), nrow = 3, ncol = 3))
 test$getMatrix()
    [,1] [,2] [,3]
1,]   10   40   70
2,]   20   50   80
3,]   30   60   90
 # check if inverse cache is empty.
 test$getInverse()
NULL

 # override matrix
 test$setMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2))
 test$getMatrix()
     [,1] [,2]
[1,]    1    3
[2,]    2    4

# test cache inverse
 test$getInverse()
NULL
 cacheSolve(test)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
 test$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
 # test retrieve message...
 cacheSolve(test)
# getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
 # reset matrix and cache...
> test$setMatrix(matrix(c(20,30,40,50,60,70,80,10,90), nrow = 3, ncol = 3))
> test$getMatrix()
     [,1] [,2] [,3]
[1,]   20   50   80
[2,]   30   60   10
[3,]   40   70   90
> cacheSolve(test)
             [,1]        [,2]         [,3]
[1,] -0.104444444 -0.02444444  0.095555556
[2,]  0.051111111  0.03111111 -0.048888889
[3,]  0.006666667 -0.01333333  0.006666667
> test$getInverse()
             [,1]        [,2]         [,3]
[1,] -0.104444444 -0.02444444  0.095555556
[2,]  0.051111111  0.03111111 -0.048888889
[3,]  0.006666667 -0.01333333  0.006666667
> 
