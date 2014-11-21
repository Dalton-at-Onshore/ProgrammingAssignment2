## The following two functions will cache the inverse of a matrix

## makeCacheMatrix will create a matrix object that can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        ##set matrix
        set <- function(y){
                x <<- y
                inv <- NULL
        }
        
        ##get matrix
        get <- function() x
        
        ##set matrix inverse
        setInverse <- function(inv) {i <<- inv}
        
        ##get matrix inverse
        getInverse <- function() i
        
        ##return list
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Find the inverse of the matrix object created above.If the inverse
## has already been calculated and the matrix has not changed then 
## cacheSolve should return getInverse 

cacheSolve <- function(x, ...) {
        ## return i if previously cached
        i <- x$getInverse
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        ## get stored matrix
        data <- x$get()
        
        ## calculate inverse
        i <- solve(data)
        
        ## set inverse
        x$setInverse(i)
        
        ##return matrix
        i
}
