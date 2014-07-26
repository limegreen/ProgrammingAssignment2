## For a given matrix "bob", these functions can be used to store the inverse matrix
## of "bob", so that rather than recalculating the inverse on the fly, if the inverse
## matrix has already been calculated, a stored value is used instead.
## To use this, first create a special matrix using the makeCacheMatrix() function
##      special.bob <- makeCacheMatrix(bob)
## Then, instead of using solve() to calculate the inverse of bob, use solveCache()
##      solveCache(special.bob)  #calculates inverse of bob or uses stored value if available


## makeCacheMatrix stores a matrix, such that its inverse is stored and accessible 
## from the solveCache function

makeCacheMatrix <- function(x = matrix()) {  #function takes a matrix as only argument
  minv <- NULL                               #stores empty inverse matrix as default
  set <- function(y) {                       #sets the value of the matrix in the parent function
    x <<- y
    minv <<- NULL
  }
  get <- function() x                        #returns the original matrix
  setinv <- function(inv) minv <<- inv       #assigns inverse value to parent function
  getinv <- function() minv                  #retrieves inverse value
  list(set = set, get = get,                 #returns the special matrix
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve() is a replacement function for solve() that for a given matrix
## created by the preceding function, stores the inverse value of the matrix

cacheSolve <- function(x, ...) {
  minv <- x$getinv()                         #retrives stored value from special matrix
  if(!is.null(minv)) {                       #checks if stored value is empty
    message("getting cached data")           #if not empty, reports that retrieving stored inverse
    return(minv)                             #returns stored inverse
  }                                          #else (ie stored value is empty)
  data <- x$get()                            #retrieves original matrix
  minv <- solve(data, ...)                   #calculates inverse
  x$setinv(minv)                             #stores inverse
  minv                                       #returns inverse
}



### TEST CODE ###
#set.seed(42)
#bob <- matrix(rnorm(16), 4, 4)             #produces small random matrix
#special.bob <- makeCacheMatrix(bob)        #produces special matrix
#cacheSolve(special.bob)                    #produces inverse
#cacheSolve(special.bob)                    #produces inverse again which should retrive 
                                            #stored value, so should see "getting cached
                                            #data" message
### BOB IS SPECIAL ###
