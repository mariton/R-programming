
## makeCacheMatrix defines an object
## makeCacheMatrix class has four properties (set/get - sets or gets matrix value and setInv/getInv - sets or gets the inverse)

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      
      #sets the inverse of a matrix
      setInv <- function(solve) m <<- solve
      
      #gets the inverse of a matrix
      getInv <- function() m
      
      list(set = set, get = get,
           setInv = setInv,
           getInv = getInv)
}


#cacheSolve function uses makeCacheMatrix objects and returns the inverse or 
#itself if the matrix is not a square one

cacheSolve <- function(x, ...) {
      
      #gets the initial matrix
      data <- x$get()  
      #if the matrix is square then calculate the inverse
      if(nrow(data)==ncol(data))
      {
            m <- x$getInv()
            if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
            }
            
           
            m <- solve(data, ...)
            x$setInv(m)
            return(m)
      }
      else 
      {
            message("matrix not square!so isn't invertible!")
                  
      }
}

#the calling commands
#source("cachematrix.R")
#be mat the giving matrix
#mat<-matrix(c(2,3,2,2), nrow=2,ncol=2,byrow=TRUE)
#objCached<-makeCacheMatrix(mat)
#cacheSolve(objCached)
#if you call objCached$getInv() you will get the inverse of the giving matrix

