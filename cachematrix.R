## Those two functions allow to easily store and retrieve the inverse
## of a matrix by storing it into the caseh

## makeCacheMatrix is a function that creates a special 'matrix' object 
## that can cache its inverse, this special 'matrix' object is in fact
## a list of four functions to 1) set, 2) get the matrix and 3) to set 
## and 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    n <- NULL
    set <- function(y) { ## sets the matrix
        x <<- y
        n <<- NULL
    }
    get <- function() x ## gets the matrix
    setinverse <- function(inverse) n <<- inverse ## sets the inverse
    getinverse <- function() n ## gets the inverse
    list(set = set, get = get, ## returns a list of the four 'named' functions
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of the special
## 'matrix' returned by makeCacheMatrix function, if inverse has
## already been calculated, then it retrieves the cached inverse
## otherwise it calculates it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    n <- x$getinverse()
    if(!is.null(n)) { ## checks if the inverse is already cached
        message("getting cached data")
        return(n)
    }
    data <- x$get()
    n <- solve(data, ...) ## calculates the inverse if not already cached
    x$setinverse(n)
    n
    
}
