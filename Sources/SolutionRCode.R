# This function, makeVector creates a special "vector", which is really a list containing 
# a function to
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean

makeVector <- function(x = numeric()) {
      m <- NULL
      set <- function(y) {                      # 1. set the value of the vector
            x <<- y                             # the <<- operator which can be used to assign a value
                                                # to an object in an environment that is different 
                                                # from the current environment. 
            m <<- NULL
      }
      get <- function() x                       # 2. get the value of the vector
      setmean <- function(mean) m <<- mean      # 3. set the value of the mean in the cache
      getmean <- function() m                   # 4. get the value of the mean
      list(set = set, get = get,
           setmean = setmean,
           getmean = getmean)
}

# This function calculates the mean of the special "vector" created with the above function. 
# However, it first checks to see if the mean has already been calculated. If so, it gets 
# the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data
# and sets the value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
      m <- x$getmean()
      if(!is.null(m)) {                         # if mean has been already calculated
            message("getting cached data")
            return(m)                           # get the mean from the cache.
      }
      data <- x$get()                           # if not, it gets the data.                           
      m <- mean(data, ...)                      # calculate the mean of the data.
      x$setmean(m)                              # set the value of the mean in the cache.
      m
}

############
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      invMat <- NULL
      set <- function(y) {                      # 1. set the value of the matrix
            x <<- y
            invMat <<- NULL
      }
      get <- function() x                       # 2. get the value of the matrix
      setinverse <- function(inverseMatrix) invMat <<- inverse # 3. set the value of the matrix in the cache
      getinverse <- function() invMat                          # 4. get the value of the matrix
      list( set = set,
            get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}

############
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMat <- x$getinverse()
      if(!is.null(invMat)) {                          # if inverse matrix has been already calculated
            message("getting cached data")
            return(invMat)                            # get the inverse matrix from the cache.
      }
      data <- x$get()                                 # if not, it gets the data
      invMat <- solve(data, ...)                      # calculate the inverse matrix of the data.
      x$setinverse(invMat)                            # set the value of the inverse matrix in the cache.
      i
} 





