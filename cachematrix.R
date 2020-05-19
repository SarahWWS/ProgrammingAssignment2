
## Put comments here that give an overall description of what your
## functions do

## This combination of functions calculates the inverse of a matrix and stores it for retrieval so the calculations
## need not be conducted repeatedly.

## The first function creates a set of functions that will be used in the calculation and caching of the matrix inverse.

makeCacheMatrix <- function(u = matrix()) {     # Create a function and two objects, u and v, to store information in makeCacheMatrix
  v <- NULL                                     # environment. u is an empty numeric matrix and the value of v is NULL.
  set <- function(w) {                          # Define the set function, it has a single argument, y.
    u <<- w                                     # The set function assigns the value of u to w in the parent environment of z and  
    v <<- NULL                                  # it resets the value of v back to NULL in the parent environment to avoid the use
  }                                             # of the wrong cached value.
  get <- function() u                           # Define the get function, it has a singlle argument that is not defined to R retrieves it
  # from the parent environment.
  setsolve <- function(solve) v <<- solve       # Define the setsolve function to run solve, which is stored in v in the parent environment.
  getsolve <- function() v                      # Define the getsolve functioon, using an undefined vallue, so again, R retreives it from
  # the parent environment.
  list(set = set, get = get,                    # Tells R to return a list to the parent environment and to name the objects in the list.
       setsolve = setsolve,
       getsolve = getsolve)
}


## The second function uses the functions created by makeCacheMatrix to calulate and store the matrix inverse if it
## has not been calculatedd previously or recalls the cached version of the matrix inverse if it has already been
## calculated.

cacheSolve <- function(u, ...) {                # Create a function with one argument and the possibility of more.
  v <- u$getsolve()                              # Assign to v the value of the solve function in u$getsolve()
  if(!is.null(v)) {                             # if v is not NULL.
    message("get cached data")
    return(v)
  }
  data <- u$get()                               # If v is null, R retrieves the values from matrix u,
  v <- solve(data, ...)                         # conducts the solve operation on the data and assigns the result to v,
  u$setsolve(v)                                 # sets it in the parent environment, and
  v                                             # prints the result of the solve operation.
}
