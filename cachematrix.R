## Caching the inverse of a matrix, rather than compute it repeatedly,
##   can avoid computation consuming tasks and save time.
## The two functions below are used together to create a special "matrix"
##   object and to calculate and cache its inverse

####

# The 'makeCacheMatrix' function creates a special "matrix", which is really
#   a list containing a function to:
#     1. set the value of the matrix
#     2. get the value of the matrix
#     3. set the value of the inverse of the matrix
#     4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s<-NULL
  set<-function(y) {
    x<<-y
    s<<-NULL
  }
  get<-function()x
  setsolve<-function(solve) s <<- solve
  getsolve<-function() s 
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
}

# The 'cacheSolve' function checks to see if the inverse of the matrix
#   has already been calculated. If so, it gets the inverse from the cache
#   and skips the computation. Otherwise, it calculates the inverse of the
#   matrix created by 'makeCacheMatrix' and sets the value of the inverse
#   in the cache via the 'setsolve' function


cacheSolve <- function(x, ...) {
  s<-x$getsolve()
  if(!is.null(s)){
    message("getting cache data")
    return(s)
  }
  data<-x$get()
  s<-solve(data, ...)
  x$setsolve(s)
  s
}

##tests 
y<-matrix(1:4,2,2)  
y_<-makeCacheMatrix(y)
cacheSolve(y_)

m <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(m)

a <- c(1, 3, 4, 0, 2)
b <- c(6, 7, 8, 5, 62)
c <- c(98, 7, 6, 9, 8)
d <- c(54, 3, 2, 5, 4)
e <- c(1, 2, 0, 4, 1)
mat <- as.matrix(rbind(a, b, c, d, e))
m <- makeCacheMatrix(mat)
cacheSolve(m)


