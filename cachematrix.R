## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix funtion is a list of functions


makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    ## getter function of x matrix
    get <- function() x
    
    ## setInverse funtion caches the result of solve function in variable m
    setInverse <- function(solve) m <<- solve
    
    ## getInverse function is used in cacheSolve function to retrieve
    ## value of variable m
    getInverse <- function() m
    
    ## list of functions tag = value pairs
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)

}


## Write a short comment describing this function
## Function cacheSolve computes the inverse of a given matrix if the value
## was not cached in previous calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Getting stored value via getInverse() function
    m <- x$getInverse()
    
    ## if statement checks if the value of m is not null
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    ## if value of m is null we get matrix from makeCacheMatrix function 
    data <- x$get()
    
    m <- solve(data)
    
    
    ## caching solved matrix in m variable
    x$setInverse(m)
    m
}
