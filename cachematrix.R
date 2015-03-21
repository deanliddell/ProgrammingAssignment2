# ____________________________________________________________________________ #
# INTRODUCTION                                                                 #
#                                                                              #
# In this assignment we write an R function that is able to cache potentially  #
# time-consuming computations. For example, taking the mean of a numeric vec-  #
# tor is typically a fast operation. However, for a very long vector, it may   #
# take too long to compute the mean, especially if it has to be computed re-   #
# peatedly (e.g. in a loop). If the contents of a vector are not changing, it  #
# may make sense to cache the value of the mean so that when we need it again, #
# it can be looked up in the cache rather than recomputed. In this assignment  #
# we take advantage of the scoping rules of the R language and how they can be #
# manipulated to preserve state inside of an R object.                         #
#                                                                              #
# ____________________________________________________________________________ #
# CACHING THE MEAN OF A VECTOR                                                 #
#                                                                              #
# In this program assignment we introduce the <<- operator which can be used   #
# to assign a value to an object in an environment that is different from the  #
# current environment. Below are two functions, makeCacheMatrix and cacheSolve #
# that are used to create a special object that stores a numeric matrix and    #
# cache's its mean.                                                            #
#                                                                              #
# CACHING THE INVERSE OF A MATRIX                                              #
#                                                                              #
# Matrix inversion is usually a costly computation and there may be some be-   #
# nefit to caching the inverse of a matrix rather than compute it repeatedly.  #
# Computing the inverse of a square matrix can be done with the solve func-    #
# tion in R. For example, if X is a square invertible matrix, then solve(X)    #
# returns its inverse.                                                         #
# ____________________________________________________________________________ #



# ____________________________________________________________________________ #
#                                                                              #
# makeCacheMatrix                                                              #
#                                                                              #
# This function receives a 'matrix' argument and creates a number of sub-      #
# functions (methods) that get and set the matrix in the parent (closure)      #
# (closure) environment. These methods are returned to the caller in a list    #
# containing the functions that:                                               #
#   1)  Set the value of the matrix;                                           #
#   2)  Get the value of the matrix;                                           #
#   3)  Set the value of the inverse; and                                      #
#   4)  Get the value of the inverse.                                          #
# ____________________________________________________________________________ #

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL               # Initialize the cache.
    set <- function(y) {        # Assign the input matrix to 'x'
        x <<- y                 # in the parent (closure) environment.
        cache <<- NULL          # Initialize the cache.
    }
    get <- function() x         # Return the matrix.
                                # Set the cache to the inverse of matrix 'x'.
    set.inverse <- function(inverse) cache <<- inverse
                                # Return the matrix inverse of 'x'.
    get.inverse <- function() cache
                                # Return the list of methods to the caller.
    list( set = set,
          get = get,
          set.inverse = set.inverse,
          get.inverse = get.inverse)
}

# ____________________________________________________________________________ #
#                                                                              #
# cacheSolve                                                                   #
#                                                                              #
# This function receives a 'function' in the argument. The function queries    #
# the cache to determine if data is present. If true, then the cache, which    #
# will contain the matrix inverse, is returned to the caller. If false, the    #
# function retrieves the matrix contained in the argument, uses the R solve()  #
# function to calculate the matrix inverse, and calls the set.inverse method   #
# to store the results in the cache.                                           #
#_____________________________________________________________________________ #

cacheSolve <- function(x, ...) {
    cache <- x$get.inverse()    # Get the matrix inverse from the cache.
    if(!is.null(cache)) {       # If the cache is not NULL, then send a
                                # diagnostic message to the user and return
                                # the cache to the caller.
        message("Getting cached data...")
        return(cache)
    }
    data <- x$get()             # If the cache is NULL, then get the matrix
                                # in the function argument.
    cache <- solve(data, ...)   # Call the R solve() function to calculate the
                                # matrix inverse.
    x$set.inverse(cache)        # Call the set method to put the matrix
                                # inverse in the parent (closure) environment.
    cache                       # Return the cache to the caller.
}
