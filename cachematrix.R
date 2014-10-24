## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Creates a list of functions that cache's the inverse of matrix
  inv <- NULL # initializing inverse variable
  # Set & get functions defined to set and get the values
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  # Set & get inverse functions defined to set and get the inverse values
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # list of functions created in makeCacheMatrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## Will return inverse of matrix using makeCahceMatrix, if already calculated and matrix not changed,
## then retreives the earlier calculated value than calculating it again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # If already calculated, then it retrieves from cache
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  # Error handling to avoid calculating inverse for singular matrix  
  a <- det(x$get())
  
  if(a == 0){
    stop("determinant is 0, cannot calculate Inverse")
  } 
  else {
    # Returns a matrix that is the inverse of 'x'by makeCacheMatrix unless inverse has already been calculated  
    m <- solve(x$get())
    x$setinverse(m)
    m
  }
}

#Test cases - uncomment to run (## below indicates results displayed)

# Sample run:
# x <-  matrix(c(1,3,2,4,5,6,8,7,4), nrow = 3)
# m = makeCacheMatrix(x)
# m$get() #to display the matrix

##      [,1] [,2] [,3]
##[1,]    1    4    8
##[2,]    3    5    7
##[3,]    2    6    4

# No cache in the first run
# cacheSolve(m)

##      [,1]  [,2]  [,3]
##[1,] -0.44  0.64 -0.24
##[2,]  0.04 -0.24  0.34
##[3,]  0.16  0.04 -0.14

# Retrieving from the cache in the second run
# cacheSolve(m)

## getting cached data.

##      [,1]  [,2]  [,3]
##[1,] -0.44  0.64 -0.24
##[2,]  0.04 -0.24  0.34
##[3,]  0.16  0.04 -0.14

# x <- matrix(1:16,nrow=4)
# m <- makeCacheMatrix(x)
# cacheSolve(m)

## Error in cacheSolve(m) : detriminant is 0, cannot calculate Inverse

