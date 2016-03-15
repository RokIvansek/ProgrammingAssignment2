## The following two functions make use of the scoping rules of the R language
## to preserve states inside an R object

## This function constructs a special object (but actualy a list :),
## that contains functions (methods) for:
##    -setting the value of the matrix,
##    -getting the value of the matrix,
##    -setting the value of the invers,
##    -getting the value of the invers.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y #assigning x the value y
    i <<- NULL #if we change the data the old invers won't do anymore
  }
  get <- function() x #just returns the data
  setinvers <- function(solve) i <<- solve #set the invers
  getinvers <- function() i #return the invers
  list(set = set, get = get, #store these 4 functions in a list
       setinvers = setinvers,
       getinvers = getinvers)

}

## This function checks to see if the invers of the matrix has allready
## been calculated and if it hasn't been it calculates it and updates the
## special object given in input.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinvers() #store what is stored as an invers in variable i
  if(!is.null(i)) { #if it is not null (invers available)
    message("getting cached data")
    return(i) #return it
  }
  #else
  data <- x$get() #get data
  i <- solve(data, ...) #compute the invers
  x$setinvers(i) #store it in the special matrix
  i
}
