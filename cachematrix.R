## The function below generates the inverse of a matrix, and is divided in two
## mains parts: makeCacheMatrix and cacheSolve

## First, as requested, the makeCacheMatrix function, getting a matrix x
## and caching its inverse
makeCacheMatrix <- function(x = matrix()) { 
  ## sets "i" as NULL
  i <- NULL 
  ## now, set will attribute to "x", in all environments, the values of "y"
  ## basically, it uses the <<- operator to work in different environments
  set <- function(y) {
    x <<- y
    i <<- NULL
  } 
  ## get will simply grab the values of x, previously overwritten by "y", and
  ## only be used in the end
  get <- function() x
  ## inverseset sets i in the different environments using the operator <<-,
  ## whereas inverseget grabs the i established before
  inverseset <- function(inverse) i <<- inverse
  inverseget <- function() i
  ## now, a list is generated, consisting of 4 itens... each item is its
  ## respective function
  list (set = set,
        get = get,
        inverseset = inverseset,
        inverseget = inverseget)
}


## Then, we have the cacheSolve function

cacheSolve <- function(x, ...) {
  ## it defines i as x in inverseget function, and if i is null it returns a 
  ## message, getting data previously cached
  i <- x$inverseget()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  ## data takes x from get
  data <- x$get()
  ## i inverses the matrix obtained by data
  i <- solve(data, ...)
  ## inverseset is called, defining "i" as the inverse in all environments
  x$inverseset(i)
  i
}