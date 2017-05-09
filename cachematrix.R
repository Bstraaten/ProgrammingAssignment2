# programming assignment week 3

## example code
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## refresh on taking the inverse of a Matrix

### create practice matrix
c=rbind(c(1, -1/4), c(-1/4, 1))
### check class, determinant and inverse
class(c)
det(c)
solve(c)
### check if we get identity matrix when multiplying inverse with matrix
solve(c) %*% c

## construct function from example code that creates matrix object that can cache its inverse        
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## construct function from example code that calcreates matrix object that can cache its inverse
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## input matrix c in makeCacheMatrix and create object x
x <- makeCacheMatrix(c)
## input x in cacheSolve 
cacheSolve(x)
## input x in cacheSolve again to see if it uses cached value
cacheSolve(x)

# check if inverse is correct
solve(c)
cacheinverse(x) == solve(c)
cacheinverse(x) %*% c



