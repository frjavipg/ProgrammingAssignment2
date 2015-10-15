# The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment

## makeCacheMatrix creates and returns a list of functions

makeCacheMatrix = function(x = matrix()) {
   m = NULL
   set = function(y) {
      x <<- y
      m <<- NULL
   }
   get = function() x
   setmatrix = function(matrix) m <<- matrix
   getmatrix = function() m
   list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix

cacheSolve = function(x, ...) {
   m = x$getmatrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data = x$get()
   m = try(solve(data, ...), silent = TRUE)
   if(is.matrix(m)){
      x$setmatrix(m)
      m
   }else
      print("No se puede obtener matriz inversa, proporcione otra matriz")
}


# load R porgram

#source("cachematrix.R")

a <- makeCacheMatrix() 

# create matrix in working environment
a$set(matrix(1:4, 2, 2))

# 1st run returns inverted matrix from working environment
cacheSolve(a)
matrix(1:4, 2, 2)

