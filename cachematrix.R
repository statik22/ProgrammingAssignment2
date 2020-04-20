## The functions below allow to set the inverse of a square invertible matrix
## If the matrix has not changed, and the inverse is stored in cache: Value from the cache is displayed as output
## If the matrix has changed: A new inverse is calculated, stored in the cache and also displated as output

## This function creates a "special" matrix which is a list allowing different functions on a square invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #Declaring the inverse object to store the value
  inv <- NULL
  
  #Declaring the object to store if the matrix has changed or not
  changed <- TRUE
  
  #In case of a new matrix, Setting the values and the marking the matrix changed as "TRUE"
  #This matrix changed as "TRUE" can be checked to find if a new inverse needs to be calculated
  set <-function(y) {
    
    x <<- y
    inv <<- NULL
    changed <<- TRUE
  }
  
  #To get the matrix stored for the inverse calculation
  get <- function() x
  
  #Storing the inverse of the new matrix in the cache and marking the matrix changed as "FALSE"
  #Matrix changed as "FALSE" informs that the inverse in cache is now a new one
  invx <- function(x) {
    
    inv <<- solve(x)
    changed <<- FALSE
  }
  
  #Display the inverse stored in cache
  getinvx <- function() inv
  
  #Check if the matrix is changed or not since last inverse was stored on cache 
  chngx <- function() changed
  
  list(set = set, 
       get = get ,
       getinvx = getinvx,
       invx = invx, 
       chngx = chngx )
}


## This function displays the inverse of the matrix based on below logic:
## If the matrix has not changed, and the inverse is stored in cache: Value from the cache is displayed as output
## If the matrix has changed: A new inverse is calculated, stored in the cache and also displated as output

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  #Checking if the matrix has changed since the last inverse was stored on cache
  check <- x$chngx()
  
  if(check == FALSE) {
    
    #FALSE means the inverse in cache is correct and can be displayed
    message ("Retrieveing value from cache")
    message ("Inverse is:")
    
    x$getinvx()
    
  } else {
    
    #TRUE means the matrix has been changed since last inverse was calculated, and hence a new inverse is to be calculated 
    message ("Matrix has changed, calculating new value and Storing in cache")
    message ("Inverse is:")
    
    newmat <- x$get()
    x$invx(newmat)
    x$getinvx()
  }
  
}