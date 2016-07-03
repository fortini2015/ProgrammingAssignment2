## These two function create, store and recall a matrix and its inverse in/from cache 

## makeCacheMatrix creates custom matrix type containing four functions:
## set stores the matrix in cache, get recalls the matrix
##setInverse and getInverse do the same but for the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()){    # the input has to be a matrix
          m <- NULL
          set <- function(y){
                    x <<- y  # stores the "real"matrix in cache
                    m <<- NULL # create empty  cache 
                  }
          get <- function() x # get matrix
          setInverse <- function(solve) m<<- solve #set inverse matrix, applying solve to x and store it in m
          getInverse <- function() m #get inverse matrix, access m to recall it
          list(set = set, get = get,
                      setInverse = setInverse,
                      getInverse = getInverse)  # create list of functions
}

#cacheSolve take a custom matrix type created by the makeCacheMatrix function
# and calculates the inverse matrix of it,
# but first it checks to see if the calculation has been done before.
# If so it recalls the data from the cache. 
# Otherwise it calculates the inverse matrix then store it in the cache.

cacheSolve <- function(x, ...) {
        
                 ## Return a matrix that is the inverse of 'x' as output
                 m <- x$getInverse()  #query the x matrix's cache checking if it already exists
                 # in this case, IF is positive and skips conmputation ( retrieving the inverse)
                 
                 if(!is.null(m)){                #if there is a cache, the inverse has been previously calculated
                     message("getting cached data")    
                                return(m)        # returning the cached calculation
                              }
                          data <- x$get()                     # get the matrix used by makeCacheMatrix function 
                          m <- solve(data, ...)               # calculate the inverse of the matrix with SOLVE and stores it in m
                          x$setInverse(m)                     # store the inverse matrix in cache using the makeCacheMatrix set function
}
